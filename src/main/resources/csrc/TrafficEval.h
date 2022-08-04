#ifndef __CONSTELLATION_TRAFFICEVAL_H
#define __CONSTELLATION_TRAFFICEVAL_H

#include <cstddef>
#include <vector>
#include <string>
#include <queue>
#include <list>
#include <map>
#include <random>
#include <cassert>

extern "C" {
#include "netrace.h"
}

class flit_t
{
public:
 flit_t(bool head, bool tail,
	uint64_t ingress_id, uint64_t egress_id,
	int64_t unique_id, uint64_t creation_cycle)
   : head(head), tail(tail), ingress_id(ingress_id), egress_id(egress_id), unique_id(unique_id), creation_cycle(creation_cycle) { }

  bool head;
  bool tail;
  uint64_t ingress_id;
  uint64_t egress_id;
  uint64_t unique_id;
  uint64_t creation_cycle;
};

typedef struct flow_rate_t {
  uint64_t ingress_id;
  uint64_t egress_id;
  float rate;
} flow_rate_t;



class runtime_params_t
{

public:
  runtime_params_t(std::vector<std::string> args);

  uint64_t num_ingresses;
  uint64_t num_egresses;

  /* Number of cycles spent warming up */
  uint64_t warmup_cycles;
  /* Number of cycles spent measuring network */
  uint64_t measurement_cycles;
  /* Maximum cycles spent in drain phase. */
  uint64_t drain_timeout_cycles;
  /* Possible flows and rates */
  std::vector<flow_rate_t> flow_rates;
  /* Static number of flits per packet. */
  uint64_t flits_per_packet;

  float required_throughput;
  uint64_t required_median_latency;
  uint64_t required_max_latency;

  /* use netrace-generated traces */
  bool netrace_enable;
  int netrace_region;
  std::string netrace_trace;

  bool netrace_ignore_dependencies;

  bool in_warmup(uint64_t cycle) {
    return cycle < warmup_cycles;
  }

  bool in_measurement(uint64_t cycle) {
    return !in_warmup(cycle) && cycle < warmup_cycles + measurement_cycles;
  }
  bool in_drain(uint64_t cycle) {
    return !in_warmup(cycle) && !in_measurement(cycle);
  }

  bool timed_out(uint64_t cycle) {
    return cycle > warmup_cycles + measurement_cycles + drain_timeout_cycles;
  }
};

class traffic_eval_t
{
public:
  traffic_eval_t(runtime_params_t *params);

  virtual flit_t* ingress_tick(uint64_t ingress_id, uint64_t current_cycle,
		       char ready,
		       bool gen_packets,
		       bool count_sent_flits) = 0;
  virtual void egress_tick(uint64_t egress_id,
			   bool* ready, bool valid, bool head, bool tail,
			   uint64_t ingress_id, uint64_t unique_id,
			   uint64_t current_cycle, bool count_recvd_flits
		   ) = 0;
  void reset_packets_received();
  bool no_inflight_flits() { return inflight_flits.empty(); };
  uint64_t num_inflight_flits() { return inflight_flits.size(); };

  uint64_t get_flits_received(flow_rate_t& flow) {
    return this->flits_received[flow.ingress_id][flow.egress_id];
  };
  uint64_t get_flits_sent(flow_rate_t& flow) {
    return this->flits_sent[flow.ingress_id][flow.egress_id];
  };
  uint64_t get_max_latency(flow_rate_t& flow) {
    if (this->get_flits_received(flow) > 0) {
      return this->latencies_by_flow[flow.ingress_id][flow.egress_id].rbegin()->first;
    } else {
      return 0;
    }
  };
  uint64_t get_overall_max_latency() {
    if (this->total_flits_received > 0) {
      return this->latencies.rbegin()->first;
    } else {
      return 0;
    }
  };
  uint64_t get_median_latency(flow_rate_t& flow) {
    return this->get_median(this->latencies_by_flow[flow.ingress_id][flow.egress_id],
			    this->flits_received[flow.ingress_id][flow.egress_id]);
  };
  uint64_t get_overall_median_latency() {
    return this->get_median(this->latencies,
			    this->total_flits_received);
  };
  uint64_t get_overall_latency_count(uint64_t latency) {
    if (this->latencies.find(latency) == this->latencies.end()) {
      return 0;
    }
    return this->latencies[latency];
  }

protected:
  uint64_t inject_flits_for_packet(uint64_t ingress_id, uint64_t egress_id,
				   bool count_injected_flits,
				   uint64_t current_cycle);
  void eject_flits(bool head, bool tail,
		   uint64_t ingress_id, uint64_t egress_id, uint64_t unique_id,
		   uint64_t current_cycle, bool count_recvd_flits);

  uint64_t get_median(std::map<uint64_t,uint64_t> &hist, uint64_t total) {
    uint64_t running = 0;
    for (std::map<uint64_t,uint64_t>::iterator it = hist.begin(); it != hist.end(); ++it) {
      running += it->second;
      if (running >= total / 2) {
	return it->first;
      }
    }
    return 0;
  }

  // Flits per packet
  uint64_t flits_per_packet;

  // Counter generating unique flit identifiers
  uint64_t unique_flit_id;
  // Ingress queues
  std::vector<std::queue<flit_t*>> ingress_queues;
  // Map tracking all inflight flits
  std::map<uint64_t, flit_t*> inflight_flits;

  uint64_t num_ingresses;
  uint64_t num_egresses;

  // Count flits sent per flow
  std::vector<std::vector<uint64_t>> flits_sent;
  // Count flits received per flow
  std::vector<std::vector<uint64_t>> flits_received;
  uint64_t total_flits_received;
  // latency histogram
  std::vector<std::vector<std::map<uint64_t, uint64_t>>> latencies_by_flow;
  std::map<uint64_t,uint64_t> latencies;

  uint64_t get_new_unique_flit_id() { return unique_flit_id++; }
};


class random_traffic_eval_t : public traffic_eval_t
{
public:
  random_traffic_eval_t(runtime_params_t *params);

  flit_t* ingress_tick(uint64_t ingress_id, uint64_t current_cycle,
		       char ready,
		       bool gen_packets,
		       bool count_sent_flits);
  void egress_tick(uint64_t egress_id,
		   bool* ready, bool valid, bool head, bool tail,
		   uint64_t ingress_id, uint64_t unique_id,
		   uint64_t current_cycle, bool count_recvd_flits
		   );
private:
  // Utilities for random generation
  std::default_random_engine generator;
  std::uniform_real_distribution<float> egress_selector;
  // Track flows by ingress id
  std::vector<std::vector<flow_rate_t>> flows_by_ingress;
};


class netrace_traffic_eval_t : public traffic_eval_t
{
public:
  netrace_traffic_eval_t(runtime_params_t *params);

  flit_t* ingress_tick(uint64_t ingress_id, uint64_t current_cycle,
		       char ready,
		       bool gen_packets,
		       bool count_sent_flits);
  void egress_tick(uint64_t egress_id,
		   bool* ready, bool valid, bool head, bool tail,
		   uint64_t ingress_id, uint64_t unique_id,
		   uint64_t current_cycle, bool count_recvd_flits
		   );
private:

  std::vector<std::list<std::pair<nt_packet_t*, uint64_t>>> waiting_queues;
  std::map<uint64_t, nt_packet_t*> nt_packet_map;
  std::list<nt_packet_t*> dead_packets;
  nt_context_t nt_ctx;
  nt_header_t* nt_header;
  bool ignore_dependencies;
  nt_packet_t* trace_packet;
  uint64_t cycle_offset;
  uint64_t next_cycle;
};

#endif
