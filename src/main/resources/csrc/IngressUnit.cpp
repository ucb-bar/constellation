#include <cstdio>
#include <iostream>
#include <vector>
#include <cmath>

unsigned long long NUM_FLITS;
unsigned long long NUM_INGRESSES;

struct flit {
  bool head;
  bool tail;
  int egress_id;
  unsigned long long payload; // cycle_time flit was created
  // TODO
}

vector<vector<flit>>* srcQueues;

/*
 * Called at the beginning of simulation.
 * Used to initialize global state
 *
 */
extern "C" void ingressunit_init(
  unsigned long long num_flits,
  unsigned long long num_ingresses
)
{
  NUM_FLITS = num_flits;
  NUM_INGRESSES = num_ingresses;

  *srcQueues = new vector<vector<flit>> srcQueues(NUM_INGRESSES, vector<flit>());
}

/* Given an input ingress, returns the egressID of the egress to send a packet to
 * at this cycle. The 0th element of the output tuple is set to False if no packet should be
 * sent this cycle.
 */
tuple<bool, unsigned long long> gen_packet(
  unsigned long long ingress_id,
  unsigned long long cycle_count
) {
  TODO
}

extern "C" void ingressunit_tick(
  unsigned long long ingress_id,
  unsigned long long cycle_count,

  unsigned char noc_ready,
  unsigned char* flit_out_valid,
  unsigned char* flit_out_head,
  unsigned char* flit_out_tail,
  unsigned long long* flit_out_egress_id,
  unsigned long long* flit_out_payload

) {
  bool send_packet;
  unsigned long long dest;
  tie(send_packet, dest) = gen_packet(ingress_id, cycle_count);
  if (send_packet) {
    for (int i = 0; i < NUM_FLITS; i++) {
      flit& new_flit = new flit;
      new_flit.head = i == 0;
      new_flit.tail = i == (NUM_FLITS - 1);
      new_flit.egress_id = dest;
      new_flit.payload = cycle_count;
      (*srcQueues)[ingress_id].push_back(new_flit);
    }
  }

  if (noc_ready != 0) {
    flit& next_flit = (*srcQueues)[ingress_id].at(0);
    *flit_out_head = next_flit.head;
    *flit_out_tail = next_flit.tail;
    *flit_out_egress_id = next_flit.egress_id;
    *flit_out_payload = next_flit.payload;

    (*srcQueues)[ingress_id].erase(0);
  }

  return;
}