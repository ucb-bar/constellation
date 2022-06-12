#ifndef __CONSTELLATION_TRAFFICEVAL_H
#define __CONSTELLATION_TRAFFICEVAL_H

#include <cstddef>
#include <vector>
#include <string>
#include <queue>
#include <map>
#include <random>
#include <cassert>

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
  /* min throughput, if not met, causes the simulation to error */
  float min_throughput;

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

  flit_t* ingress_tick(uint64_t ingress_id, uint64_t current_cycle,
		       char ready,
		       bool gen_packets,
		       bool count_sent_flits);
  void egress_tick(uint64_t egress_id,
		   bool* ready, bool valid, bool head, bool tail,
		   uint64_t ingress_id, uint64_t unique_id,
		   bool count_recvd_flits
		   );
  bool final_stats();
  void reset_packets_received();
  bool no_inflight_flits() { return inflight_flits.empty(); };

 private:
  // Parameters for this evaluatoin run
  runtime_params_t* params;
  // Counter generating unique flit identifiers
  uint64_t unique_flit_id;
  // Ingress queues
  std::vector<std::queue<flit_t*>> ingress_queues;
  // Map tracking all inflight flits
  std::map<uint64_t, flit_t*> inflight_flits;
  // Utilities for random genreation
  std::default_random_engine generator;
  std::uniform_real_distribution<float> egress_selector;
  // Count flits sent per flow
  std::vector<std::vector<uint64_t>> flits_sent;
  // Count flits received per flow
  std::vector<std::vector<uint64_t>> flits_received;
  // Track flows by ingress id
  std::vector<std::vector<flow_rate_t>> flows_by_ingress;

  uint64_t get_new_unique_flit_id() { return unique_flit_id++; }
};

#endif
