#include <cstdio>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

unsigned long long NUM_FLITS;
unsigned long long NUM_INGRESSES;

/* C representation of a flit. */
struct flit {
  bool head;
  bool tail;
  int egress_id;
  unsigned long long payload; // cycle_time flit was created
};

/* Source Queues for input into NoC */
vector<vector<struct flit>> *srcQueues;

/*
 * Called at the beginning of simulation. Used to initialize global state.
 */
extern "C" void ingressunit_init(
  unsigned long long num_flits,
  unsigned long long num_ingresses
)
{
  NUM_FLITS = num_flits;
  NUM_INGRESSES = num_ingresses;

  srcQueues = new vector<vector<flit>>(NUM_INGRESSES, vector<flit>());
}

/* Given an input ingress, returns the egressID of the egress to send a packet to
 * at this cycle. No packet should be sent if the output is -1, otherwise cast the output to
 * unsigned long long to get the egressID for the output (this is okay because we'll never have)
 * so many egresses that unsigned long long can represent the egressID while long long can't
 */
long long gen_packet(
  unsigned long long ingress_id,
  unsigned long long cycle_count
) {
  // TODO: REPLACE THIS FAKE LOGIC WITH REAL CYCLE COUNT LOGIC
  if (cycle_count >= 2) {
    return -1;
  }
  return ingress_id + 1;
}

/* C model representing ingress units. Called every cycle. */
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
  unsigned long long dest = gen_packet(ingress_id, cycle_count);
  if (((long long) dest) != -1) {
    for (int i = 0; i < NUM_FLITS; i++) {
      flit* new_flit = new flit;
      new_flit->head = i == 0;
      new_flit->tail = i == (NUM_FLITS - 1);
      new_flit->egress_id = dest;
      new_flit->payload = cycle_count;
      (*srcQueues)[ingress_id].push_back(*new_flit);
    }
  }

  bool flit_available = (*srcQueues)[ingress_id].size() > 0;
  if (flit_available) {
    *flit_out_valid = 1;
    flit& next_flit = (*srcQueues)[ingress_id].at(0);
    *flit_out_head = next_flit.head;
    *flit_out_tail = next_flit.tail;
    *flit_out_egress_id = next_flit.egress_id;
    *flit_out_payload = next_flit.payload;
  } else {
    *flit_out_valid = 0;
  }

  if (noc_ready != 0 && flit_available) {
    (*srcQueues)[ingress_id].erase((*srcQueues)[ingress_id].begin());
  }

  return;
}

/******************/
/* Sanity Testing */
/******************/

unsigned char sf_o_v = 2;
unsigned char sf_o_h = 2;
unsigned char sf_o_t = 2;
unsigned long long s_o_e_i = 2;
unsigned long long s_o_p = 2;

int main() {
  int n_ingress = 3;
  ingressunit_init(3, n_ingress); // 3 flits per packet, 3 ingresses


  for (int i = 0; i < n_ingress; i++) { // cycle 0: each Qs packet for egress i + 1
    ingressunit_tick(i, 0, 0, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 1
      || sf_o_t != 0
      || s_o_e_i != i + 1
      || s_o_p != 0
      ) {
        printf("Failed at 0\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 1: each Qs packet for egress i + 1 but noc starts taking flits
    ingressunit_tick(i, 1, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 1
      || sf_o_t != 0
      || s_o_e_i != i + 1
      || s_o_p != 0
      ) {
        printf("Failed at 1\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 2: noc takes flits, nothing new is Qd
    ingressunit_tick(i, 2, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 0
      || sf_o_t != 0
      || s_o_e_i != i + 1
      || s_o_p != 0
      ) {
        printf("Failed at 2\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 3: noc takes flits, nothing new is Qd, packet1 tail removed
    ingressunit_tick(i, 3, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 0
      || sf_o_t != 1
      || s_o_e_i != i + 1
      || s_o_p != 0
      ) {
        printf("Failed at 3\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 4: noc takes flits, nothing new is Qd, packet2 head removed
    ingressunit_tick(i, 4, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 1
      || sf_o_t != 0
      || s_o_e_i != i + 1
      || s_o_p != 1
      ) {
        printf("Failed at 4\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 5: noc takes flits, nothing new is Qd, packet2 body removed
    ingressunit_tick(i, 5, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 0
      || sf_o_t != 0
      || s_o_e_i != i + 1
      || s_o_p != 1
      ) {
        printf("Failed at 5\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 6: noc takes flits, nothing new is Qd, packet2 head removed
    ingressunit_tick(i, 6, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 1
      || sf_o_h != 0
      || sf_o_t != 1
      || s_o_e_i != i + 1
      || s_o_p != 1
      ) {
        printf("Failed at 6\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  for (int i = 0; i < n_ingress; i++) { // cycle 6: noc takes flits, nothing to output
    ingressunit_tick(i, 7, 1, &sf_o_v, &sf_o_h, &sf_o_t, &s_o_e_i, &s_o_p);
    if ( sf_o_v != 0) {
        printf("Failed at 7\n");
        exit(-1);
    }
    sf_o_v = 2; sf_o_h = 2; sf_o_t = 2; s_o_e_i = 2; s_o_p = 2;
  }

  printf("Test Passed\n");
}