#include <cstdio>
#include <iostream>
#include <vector>
#include <cmath>
#include <cstring>
#include <random>

// https://github.com/ucb-bar/testchipip/blob/03535f56a6318236ab6abf5342d78eecf453984d/src/main/resources/testchipip/csrc/SimSerial.cc
#include <vpi_user.h>
#include <svdpi.h>

using namespace std;

char* TRAFFIC_MATRIX_PLUSARG = "+TRAFFIC_MATRIX="

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
vector<vector<struct flit>> *SRC_QUEUES;

/* TRAFFIC_MATRIX[i][e] specifies the flow rate from ingress i to egress e. */
int **TRAFFIC_MATRIX

/*
 * Called at the beginning of simulation. Used to initialize global state.
 */
extern "C" void ingressunit_init(
  unsigned long long num_flits,
  unsigned long long num_ingresses,
  unsigned long long num_egresses
)
{
  // Initialize global state
  NUM_FLITS = num_flits;
  NUM_INGRESSES = num_ingresses;

  SRC_QUEUES = new vector<vector<flit>>(NUM_INGRESSES, vector<flit>());
  TRAFFIC_MATRIX = new *int[num_ingresses];
  for (int i = 0; i < num_ingresses; i++) {
    TRAFFIC_MATRIX[i] = new int[num_egresses];
  }

  // Fill flow rate matrix
  s_vpi_vlog_info info;
  if (!vpi_get_vlog_info(&info)) {
    printf("C++ sim: Unable to get plusargs from simulator\n");
    abort();
  }

  char* filepath = NULL;
  for (int i = 0; i < info.argc; i++) {
    char* input_arg = info.argv[i];
    if (strncmp(input_arg, TRAFFIC_MATRIX_PLUSARG) != NULL) {
      filepath = input_arg + strlen(TRAFFIC_MATRIX_PLUSARG);
    }
  }
  if (filepath == NULL) {
    printf("C++ Sim: Unable to find traffic matrix filepath plusarg\n");
  }

  FILE* traffic_matrix_file = fopen(filepath, "r")
  while (feof(traffic_matrix_file) != 0) {
    int ingress; int egress; int flow_rate;
    fscanf(file, " "); // skip leading whitespace
    int num_matches = fscanf(traffic_matrix_file, "%d %d %d", &ingress, &egress, &flow_rate);
    if (num_matches != 3) {
      printf("C++ Sim: failed to read traffic matrix file (incorrect format)")
    }
    TRAFFIC_MATRIX[ingress][egress] = flow_rate;
  }
  fclose(traffic_matrix_file);
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
      (*SRC_QUEUES)[ingress_id].push_back(*new_flit);
    }
  }

  bool flit_available = (*SRC_QUEUES)[ingress_id].size() > 0;
  if (flit_available) {
    *flit_out_valid = 1;
    flit& next_flit = (*SRC_QUEUES)[ingress_id].at(0);
    *flit_out_head = next_flit.head;
    *flit_out_tail = next_flit.tail;
    *flit_out_egress_id = next_flit.egress_id;
    *flit_out_payload = next_flit.payload;
  } else {
    *flit_out_valid = 0;
  }

  if (noc_ready != 0 && flit_available) {
    (*SRC_QUEUES)[ingress_id].erase((*SRC_QUEUES)[ingress_id].begin());
  }

  return;
}