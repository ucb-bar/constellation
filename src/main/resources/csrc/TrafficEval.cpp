#include "TrafficEval.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#ifndef NO_VPI
#include <vpi_user.h>
#include <svdpi.h>
#endif

runtime_params_t* params = NULL;
traffic_eval_t* eval = NULL;

/*
 * Initializes the global runtime_params_t object for one evaluation
 * config_str is the default config string for this run. If a PLUSARG
 * config file is provided, then the contents of that file will override
 * config_str
 */
void init_params(std::string config_str) {
  // config is a vector of args from the config file
  std::vector<std::string> config;

  char* filepath = NULL;
#ifndef NO_VPI
  s_vpi_vlog_info info;
  if (!vpi_get_vlog_info(&info)) {
    std::cout << "Unable to get plusargs from simulator. Aborting." << std::endl;
    exit(1);
  }
  const char* EVAL_PARAMS_PLUSARG = "+EVAL_PARAMS=";
  for (int i = 0; i < info.argc; i++) {
    char* input_arg = info.argv[i];
    if (strncmp(input_arg, EVAL_PARAMS_PLUSARG, strlen(EVAL_PARAMS_PLUSARG)) == 0) {
      filepath = input_arg + strlen(EVAL_PARAMS_PLUSARG);
      break;
    }
  }
#endif

  // Store the config (either read from file or in config_str) as a
  // istream so we can split by line
  std::istream* stream;
  std::string line;
  if (filepath == NULL) {
    std::cout << "No traffic matrix plusarg found. Falling back on default config str:" << std::endl;
    std::cout << config_str << std::endl << std::endl;
    stream = new std::stringstream(config_str);
  } else {
    std::cout << "Constructing params from " << std::string(filepath) << std::endl;
    stream = new std::ifstream(std::string(filepath));
  }

  if (stream->fail()) {
    std::cout << "Error with config string" << std::endl;
    exit(1);
  }

  // Split config string by lne into vector of strings to pass to constructor
  while (std::getline(*stream, line)) {
    if (line.size() > 0) { config.push_back(line); }
  }
  delete stream;
  params = new runtime_params_t(config);
}

extern "C" void ingress_tick(long long int ingress_id,
			     const char* config_str,
			     long long int current_cycle,
			     unsigned char flit_out_ready,
			     unsigned char* flit_out_valid,
			     unsigned char* flit_out_head,
			     unsigned char* flit_out_tail,
			     long long int* flit_out_egress_id,
			     long long int* flit_out_unique_id
			     ) {
  if (!params) { init_params(std::string(config_str)); }
  if (!eval) { eval = new traffic_eval_t(params); }

  // Stop generating packets in drain phase
  // Only count sent flits in measurement phase
  flit_t* flit_to_send = eval->ingress_tick(ingress_id,
					    current_cycle,
					    flit_out_ready,
					    !params->in_drain(current_cycle),
					    params->in_measurement(current_cycle));
  *flit_out_valid = flit_to_send != NULL;
  if (flit_to_send) {
    *flit_out_head = flit_to_send->head;
    *flit_out_tail = flit_to_send->tail;
    *flit_out_egress_id = flit_to_send->egress_id;
    *flit_out_unique_id = flit_to_send->unique_id;
  }
}

extern "C" void egress_tick(long long int egress_id,
			    const char* config_str,
			    long long int current_cycle,
			    unsigned char* flit_in_ready,
			    unsigned char flit_in_valid,
			    unsigned char flit_in_head,
			    unsigned char flit_in_tail,
			    long long int flit_in_ingress_id,
			    long long int flit_in_unique_id,
			    unsigned char* success,
			    unsigned char* fatal
			    ) {
  if (!params) { init_params(std::string(config_str)); }
  if (!eval) { eval = new traffic_eval_t(params); }

  // Only count received flits in measurement phase
  eval->egress_tick(egress_id,
		    (bool*)flit_in_ready,
		    flit_in_valid,
		    flit_in_head,
		    flit_in_tail,
		    flit_in_ingress_id,
		    flit_in_unique_id,
		    params->in_measurement(current_cycle)
		    );

  *success = 0;
  *fatal = 0;
  if (egress_id == 0) {
    if (params->timed_out(current_cycle)) {
      std::cout << "Error, traffic eval timed out" << std::endl;
      *fatal = 1;
    } else if (params->in_drain(current_cycle) && eval->no_inflight_flits()) {
      bool error = !eval->final_stats();
      *success = !error;
      *fatal = error;
    }
  }
}

/*
 * Construct a runtime_params_t object from a config string
 * Example config string:
 *
 *  warmup           5000
 *  measurement      10000
 *  drain            100000
 *  flits_per_packet 4
 *  min_throughput   1.0
 *  flow             0 0 0.5
 *  flow             0 1 0.5
 */
runtime_params_t::runtime_params_t(std::vector<std::string> args) {
  this->warmup_cycles = 1000;
  this->measurement_cycles = 2000;
  this->drain_timeout_cycles = 500;
  this->flits_per_packet = 4;
  this->num_ingresses = 0;
  this->num_egresses = 0;
  this->min_throughput = 0.0f;

  for (std::string arg : args) {
    std::istringstream ss(arg);
    std::vector<std::string> argv;
    std::string word;
    while (ss >> word) { argv.push_back(word); }

    std::string flag = argv[0];
    if (flag == "warmup") {
      assert(argv.size() == 2);
      this->warmup_cycles = stoi(argv[1]);
    } else if (flag == "measurement") {
      assert(argv.size() == 2);
      this->measurement_cycles = stoi(argv[1]);
    } else if (flag == "drain") {
      assert(argv.size() == 2);
      this->drain_timeout_cycles = stoi(argv[1]);
    } else if (flag == "flits_per_packet") {
      assert(argv.size() == 2);
      this->flits_per_packet = stoi(argv[1]);
    } else if (flag == "min_throughput") {
      assert(argv.size() == 2);
      this->min_throughput = stof(argv[1]);
    } else if (flag == "flow") {
      assert(argv.size() == 4);
      flow_rate_t new_flow;
      new_flow.ingress_id = stoi(argv[1]);
      new_flow.egress_id = stoi(argv[2]);
      this->num_ingresses = std::max(this->num_ingresses, new_flow.ingress_id + 1);
      this->num_egresses = std::max(this->num_egresses, new_flow.egress_id + 1);
      new_flow.rate = stof(argv[3]);
      this->flow_rates.push_back(new_flow);
    } else {
      std::cout << "Error parsing config" << std::endl;
      for (std::string& s : args) {
	std::cout << s << std::endl;
      }
      exit(1);
    }
  }
  if (this->flow_rates.size() == 0) {
    std::cout << "Must specify at least one flow" << std::endl;
    exit(1);
  }
}

traffic_eval_t::traffic_eval_t(runtime_params_t *params) {
  this->ingress_queues.resize(params->num_ingresses);
  this->generator = std::default_random_engine(0xdeadbeef);
  this->egress_selector = std::uniform_real_distribution<float>(0.0, 1.0);

  for (size_t i = 0; i < params->num_ingresses; i++) {
    this->flits_received.push_back(std::vector<uint64_t>());
    this->flits_sent.push_back(std::vector<uint64_t>());
    for (size_t j = 0; j < params->num_egresses; j++) {
      this->flits_received[i].push_back(0);
      this->flits_sent[i].push_back(0);
    }
  }
  this->flows_by_ingress.resize(params->num_ingresses);
  for (flow_rate_t& flow : params->flow_rates) {
    this->flows_by_ingress[flow.ingress_id].push_back(flow);
  }
  this->params = params;
}

flit_t* traffic_eval_t::ingress_tick(uint64_t ingress_id,
				     uint64_t current_cycle, char ready,
				     bool gen_packets,
				     bool count_sent_flits
				     ) {
  // If in the packet generation phase, use a uniform random distribution
  // to insert packets. Note this phase may generate many packets in a
  // single cycle.

  // Vector of egresses to generate a packet to
  std::vector<uint64_t> to_emit;
  if (gen_packets) {
    for (flow_rate_t &flow : this->flows_by_ingress[ingress_id]) {
      float sample = this->egress_selector(this->generator);
      if (sample * this->params->flits_per_packet < flow.rate) {
	to_emit.push_back(flow.egress_id);
      }
    }
  }

  // For each packet that we generate this cycle, construct the flits
  // and enqueue them in the ingress queue for this ingress point
  std::queue<flit_t*> *ingress_q = &this->ingress_queues[ingress_id];
  for (uint64_t &egress_id : to_emit) {
    for (size_t i = 0; i < this->params->flits_per_packet; i++) {
      uint64_t unique_id = this->get_new_unique_flit_id();
      flit_t* f = new flit_t(i == 0, i + 1 == this->params->flits_per_packet,
			     ingress_id, egress_id, unique_id, current_cycle);
      this->inflight_flits[unique_id] = f;
      ingress_q->push(f);
    }
    if (count_sent_flits) {
      this->flits_sent[ingress_id][egress_id] += this->params->flits_per_packet;
    }
  }

  // Pop a flit from the head of the ingress queue to send through the network
  flit_t* deq_flit = NULL;
  if (ready && ingress_q->size() != 0) {
    deq_flit = ingress_q->front();
    ingress_q->pop();
  }
  return deq_flit;
}

void traffic_eval_t::egress_tick(uint64_t egress_id,
				 bool* ready, bool valid, bool head, bool tail,
				 uint64_t ingress_id, uint64_t unique_id,
				 bool count_recvd_flits
				 ) {
  *ready = true;

  if (valid) {
    flit_t* f = this->inflight_flits[unique_id];
    if (!f) {
      std::cout << "Lost flit " << unique_id << std::endl;
      exit(1);
    }
    assert(f->head == head);
    assert(f->tail == tail);
    assert(f->ingress_id == ingress_id);
    assert(this->inflight_flits.erase(unique_id));
    if (count_recvd_flits) {
      this->flits_received[ingress_id][egress_id]++;
    }
    delete f;
  }
}

bool traffic_eval_t::final_stats() {
  float min_throughput = std::numeric_limits<float>::max();
  flow_rate_t* min_flow = NULL;
  std::cout << "Results CSV:" << std::endl;
  std::cout << "ingress_id, egress_id, received, sent, throughput" << std::endl;
  for (flow_rate_t& flow : this->params->flow_rates) {
    uint64_t received = this->flits_received[flow.ingress_id][flow.egress_id];
    uint64_t sent = this->flits_sent[flow.ingress_id][flow.egress_id];
    float throughput = (float)received / (float)sent;
    if (throughput < min_throughput) {
      min_throughput = throughput;
      min_flow = &flow;
    }
    std::cout << flow.ingress_id << ", "
	      << flow.egress_id << ", "
	      << received << ", "
	      << sent << ", "
	      << std::to_string(throughput)
	      << std::endl;
  }
  return min_throughput >= params->min_throughput;
}
