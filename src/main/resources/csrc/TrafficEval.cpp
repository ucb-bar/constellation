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
  const char* EVAL_PARAMS_PLUSARG = "+eval_params=";
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
    std::cout << "No traffic matrix plusarg found. Falling back on default config str." << std::endl;
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

void init_eval() {
  assert(params && !eval);
  if (params->netrace_enable) {
    eval = new netrace_traffic_eval_t(params);
  } else {
    eval = new random_traffic_eval_t(params);
  }
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
  if (!eval) { init_eval(); }

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
  if (!eval) { init_eval(); }

  // Only count received flits in measurement phase
  eval->egress_tick(egress_id,
		    (bool*)flit_in_ready,
		    flit_in_valid,
		    flit_in_head,
		    flit_in_tail,
		    flit_in_ingress_id,
		    flit_in_unique_id,
		    current_cycle,
		    params->in_measurement(current_cycle)
		    );

  *success = 0;
  *fatal = 0;
  if (egress_id == 0) {
    if (params->timed_out(current_cycle)) {
      std::cout << "Error, traffic eval timed out" << std::endl;
      *fatal = 1;
    } else if (params->in_drain(current_cycle) && eval->no_inflight_flits()) {
      float min_throughput = std::numeric_limits<float>::max();
      flow_rate_t* min_flow = NULL;
      std::cout << "Results CSV:" << std::endl;
      std::cout << "ingress_id, egress_id, received, sent, throughput, median_latency, max_latency" << std::endl;
      std::map<uint64_t,uint64_t> aggregate_latency;
      for (flow_rate_t& flow : params->flow_rates) {
	uint64_t received = eval->get_flits_received(flow);
	uint64_t sent = eval->get_flits_sent(flow);
	float throughput = (float)received / (float)sent;
	if (throughput < min_throughput || !min_flow) {
	  min_throughput = throughput;
	  min_flow = &flow;
	}
	uint64_t median_latency = eval->get_median_latency(flow);
	uint64_t max_latency = eval->get_max_latency(flow);
	std::cout << flow.ingress_id << ", "
		  << flow.egress_id << ", "
		  << received << ", "
		  << sent << ", "
		  << std::to_string(throughput) << ", "
		  << median_latency << ", "
		  << max_latency
		  << std::endl;
      }
      uint64_t max_latency = eval->get_overall_max_latency();
      uint64_t median_latency = eval->get_overall_median_latency();
      std::cout << std::endl
		<< "Min throughput: "
		<< min_flow->ingress_id << ", "
		<< min_flow->egress_id << ", "
		<< min_throughput
		<< std::endl
		<< "Median latency: "
		<< median_latency
		<< std::endl
		<< "Max latency: "
		<< max_latency
		<< std::endl
		<< "Latency hist: ";
      size_t bucket_size = 10;
      for (uint64_t i = 0; i < max_latency; i += bucket_size) {
	uint64_t c = 0;
	for (uint64_t j = i; j < i + bucket_size; j++) {
	  c += eval->get_overall_latency_count(j);
	}
	std::cout << "  " << i << "-" << i + bucket_size << ": " << c << std::endl;
      }

      bool error = false;
      if (min_throughput < params->required_throughput) {
	std::cout << min_throughput << " < " << params->required_throughput << std::endl;
	error = true;
      }
      if (median_latency > params->required_median_latency) {
	std::cout << median_latency << " > " << params->required_median_latency << std::endl;
	error = true;
      }
      if (max_latency > params->required_max_latency) {
	std::cout << max_latency << " > " << params->required_max_latency << std::endl;
	error = true;
      }
      *success = !error;
      *fatal = error;
    }
  }
}

/*
 * Construct a runtime_params_t object from a config string
 * Example config string:
 *
 *  warmup                  5000
 *  measurement             10000
 *  drain                   100000
 *  flits_per_packet        4
 *  required_throughput     1.0
 *  required_median_latency 99999
 *  required_max_latency    99999
 *  netrace_enable          false
 *  netrace_trace           blackscholes_64c_simsmall.tra.bz2
 *  netrace_region          0
 *  netrace_ignore_dependencies false
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
  this->required_throughput = 0.0f;
  this->required_median_latency = 99999;
  this->required_max_latency = 99999;
  this->netrace_enable = false;
  this->netrace_trace = "blackscholes_64c_simsmall.tra.bz2";
  this->netrace_ignore_dependencies = false;

  for (std::string arg : args) {
    std::istringstream ss(arg);
    std::vector<std::string> argv;
    std::string word;
    while (ss >> word) { argv.push_back(word); }

    std::string flag = argv[0];
    if (argv[0][0] == '#') {
      continue;
    } else if (flag == "warmup") {
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
    } else if (flag == "required_throughput") {
      assert(argv.size() == 2);
      this->required_throughput = stof(argv[1]);
    } else if (flag == "required_median_latency") {
      assert(argv.size() == 2);
      this->required_median_latency = stoi(argv[1]);
    } else if (flag == "required_max_latency") {
      assert(argv.size() == 2);
      this->required_max_latency = stoi(argv[1]);
    } else if (flag == "netrace_enable") {
      assert(argv.size() == 2);
      this->netrace_enable = argv[1] == "true";
    } else if (flag == "netrace_trace") {
      assert(argv.size() == 2);
      this->netrace_trace = argv[1];
    } else if (flag == "netrace_ignore_dependencies") {
      assert(argv.size() == 2);
      this->netrace_ignore_dependencies = argv[1] == "true";
    } else if (flag == "netrace_region") {
      assert(argv.size() == 2);
      this->netrace_region = stoi(argv[1]);
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
  if (this->flow_rates.size() == 0 && !this->netrace_enable) {
    std::cout << "Must specify at least one flow" << std::endl;
    exit(1);
  }
}

traffic_eval_t::traffic_eval_t(runtime_params_t *params) {
  this->flits_per_packet = params->flits_per_packet;
  this->num_ingresses = params->num_ingresses;
  this->num_egresses = params->num_egresses;
  this->inflight_flits = std::map<uint64_t,flit_t*>();
  this->unique_flit_id = 0;
  this->total_flits_received = 0;
  for (size_t i = 0; i < params->num_ingresses; i++) {
    this->ingress_queues.push_back(std::queue<flit_t*>());
    this->flits_received.push_back(std::vector<uint64_t>());
    this->flits_sent.push_back(std::vector<uint64_t>());
    this->latencies_by_flow.push_back(std::vector<std::map<uint64_t,uint64_t>>());
    for (size_t j = 0; j < params->num_egresses; j++) {
      this->flits_received[i].push_back(0);
      this->flits_sent[i].push_back(0);
      this->latencies_by_flow[i].push_back(std::map<uint64_t,uint64_t>());
    }
  }
}

uint64_t traffic_eval_t::inject_flits_for_packet(uint64_t ingress_id, uint64_t egress_id,
						 bool count_injected_flits,
						 uint64_t current_cycle) {
  uint64_t tail_unique_id;
  for (uint64_t f = 0; f < this->flits_per_packet; f++) {
    uint64_t unique_id = this->get_new_unique_flit_id();
    flit_t *flit = new flit_t(f == 0, f + 1 == this->flits_per_packet,
			      ingress_id, egress_id, unique_id, current_cycle);
    this->inflight_flits[unique_id] = flit;
    tail_unique_id = unique_id;
    this->ingress_queues[ingress_id].push(flit);
  }
  if (count_injected_flits) {
    this->flits_sent[ingress_id][egress_id] += this->flits_per_packet;
  }
  return tail_unique_id;
}


flit_t* random_traffic_eval_t::ingress_tick(uint64_t ingress_id,
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
      if (sample * this->flits_per_packet < flow.rate) {
	to_emit.push_back(flow.egress_id);
      }
    }
  }

  // For each packet that we generate this cycle, construct the flits
  // and enqueue them in the ingress queue for this ingress point
  std::queue<flit_t*> *ingress_q = &this->ingress_queues[ingress_id];
  for (uint64_t &egress_id : to_emit) {
    inject_flits_for_packet(ingress_id, egress_id, count_sent_flits, current_cycle);
  }

  // Pop a flit from the head of the ingress queue to send through the network
  flit_t* deq_flit = NULL;
  if (ready && ingress_q->size() != 0) {
    deq_flit = ingress_q->front();
    ingress_q->pop();
  }
  return deq_flit;
}

void traffic_eval_t::eject_flits(bool head, bool tail,
				 uint64_t ingress_id, uint64_t egress_id, uint64_t unique_id,
				 uint64_t current_cycle,
				 bool count_recvd_flits) {
  flit_t* f = this->inflight_flits[unique_id];
  if (!f) {
    std::cout << "Lost flit " << unique_id << std::endl;
    exit(1);
  }
  assert(f->head == head);
  assert(f->tail == tail);
  assert(f->ingress_id == ingress_id);
  assert(this->inflight_flits.erase(unique_id) == 1);
  if (count_recvd_flits) {
    uint64_t latency = current_cycle - f->creation_cycle;
    this->flits_received[ingress_id][egress_id]++;
    this->total_flits_received++;
    this->latencies_by_flow[ingress_id][egress_id][latency]++;
    this->latencies[latency]++;
  }
  delete f;
}

void random_traffic_eval_t::egress_tick(uint64_t egress_id,
					bool* ready, bool valid, bool head, bool tail,
					uint64_t ingress_id, uint64_t unique_id,
					uint64_t current_cycle,
					bool count_recvd_flits
					) {
  *ready = true;

  if (valid) {
    eject_flits(head, tail, ingress_id, egress_id, unique_id, current_cycle, count_recvd_flits);
  }
}

random_traffic_eval_t::random_traffic_eval_t(runtime_params_t *params) : traffic_eval_t(params) {
  this->generator = std::default_random_engine(0xdeadbeef);
  this->egress_selector = std::uniform_real_distribution<float>(0.0, 1.0);
  this->flows_by_ingress.resize(params->num_ingresses);
  for (flow_rate_t& flow : params->flow_rates) {
    this->flows_by_ingress[flow.ingress_id].push_back(flow);
  }
}


netrace_traffic_eval_t::netrace_traffic_eval_t(runtime_params_t *params) : traffic_eval_t(params) {
  memset(&this->nt_ctx, 0, sizeof(nt_context_t));
  this->waiting_queues.resize(params->num_ingresses);
  this->ignore_dependencies = params->netrace_ignore_dependencies;

  assert(params->netrace_enable);
  std::cout << "Opening nettrace file " << params->netrace_trace << std::endl;
  nt_open_trfile(&this->nt_ctx, params->netrace_trace.c_str());
  if (params->netrace_ignore_dependencies) {
    nt_disable_dependencies(&this->nt_ctx);
  }
  this->nt_header = nt_get_trheader(&this->nt_ctx);
  int start_region = params->netrace_region;
  assert(start_region >= 0 && start_region < 5);
  nt_seek_region(&this->nt_ctx, &this->nt_header->regions[start_region]);
  this->cycle_offset = 0;
  for (uint64_t i = 0; i < start_region; i++) {
    this->cycle_offset += this->nt_header->regions[i].num_cycles;
  }

  this->trace_packet = nt_read_packet(&this->nt_ctx);
  this->next_cycle = 0;
}


flit_t* netrace_traffic_eval_t::ingress_tick(uint64_t ingress_id, uint64_t current_cycle,
					      char ready,
					      bool gen_packets,
					      bool count_sent_flits) {

  if (ingress_id == 0) {
    uint64_t all_ingress_q_size = 0;
    for (uint64_t i = 0; i < params->num_ingresses; i++) {
      all_ingress_q_size += this->ingress_queues[i].size();
    }
    std::cout << "Cycle: " << current_cycle << " inflight_flits: " << this->inflight_flits.size() << " " << all_ingress_q_size << std::endl;
  }
  if (gen_packets && current_cycle >= this->next_cycle) {
    // If idle, fast-forward to next flit
    if (this->trace_packet != NULL && this->trace_packet->cycle > this->cycle_offset && this->no_inflight_flits() && this->dead_packets.size() == 0) {
      this->cycle_offset = this->trace_packet->cycle;
    }

    // Get packets from tracefile, put them in waiting queue
    while (this->trace_packet != NULL && this->trace_packet->cycle <= current_cycle + this->cycle_offset) {
      if (this->trace_packet->src >= this->num_ingresses || this->trace_packet->dst >= this->num_egresses) {
	this->dead_packets.push_back(this->trace_packet);
      } else {
	std::pair<nt_packet_t*, uint64_t> pair;
	pair.first = this->trace_packet;
	pair.second = 0;
	this->waiting_queues[this->trace_packet->src].push_back(pair);
      }
      this->trace_packet = nt_read_packet(&this->nt_ctx);
    }

    // Dead packets finish instantly
    std::list<nt_packet_t*>::iterator it = this->dead_packets.begin();
    while (it != this->dead_packets.end()) {
      if (nt_dependencies_cleared(&this->nt_ctx, *it)) {
	nt_clear_dependencies_free_packet(&this->nt_ctx, *it);
	it = this->dead_packets.erase(it);
      } else {
	it++;
      }
    }


    // Move packets that have cleared dependencies from waiting queue into ingress queue
    for (uint64_t i = 0; i < this->waiting_queues.size(); i++) {
      std::list<std::pair<nt_packet_t*, uint64_t>>::iterator it = this->waiting_queues[i].begin();
      while (it != this->waiting_queues[i].end()) {
	nt_packet_t* packet = it->first;
	if (nt_dependencies_cleared(&this->nt_ctx, packet) || this->ignore_dependencies) {
	  uint64_t tail_unique_id = inject_flits_for_packet(i, packet->dst, count_sent_flits, current_cycle);
	  this->nt_packet_map[tail_unique_id] = packet;
	  it = this->waiting_queues[i].erase(it);
	} else {
	  it++;
	}
      }
    }

  }

  // Pop a flit from the head of the ingress queue to send through the network
  flit_t* deq_flit = NULL;
  std::queue<flit_t*> *ingress_q = &this->ingress_queues[ingress_id];
  if (ready && ingress_q->size() != 0) {
    deq_flit = ingress_q->front();
    ingress_q->pop();
  }
  this->next_cycle = current_cycle + 1;
  return deq_flit;
}

void netrace_traffic_eval_t::egress_tick(uint64_t egress_id,
					 bool* ready, bool valid, bool head, bool tail,
					 uint64_t ingress_id, uint64_t unique_id,
					 uint64_t current_cycle, bool count_recvd_flits
					 ) {
  *ready = true;
  if (valid) {
    if (tail) {
      nt_packet_t* packet = this->nt_packet_map[unique_id];
      assert(packet);
      nt_clear_dependencies_free_packet(&this->nt_ctx, packet);
      this->nt_packet_map.erase(unique_id);
    }
    eject_flits(head, tail, ingress_id, egress_id, unique_id, current_cycle, count_recvd_flits);
  }
}
