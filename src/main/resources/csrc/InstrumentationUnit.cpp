#include <cstdio>
#include <iostream>
#include <vector>
#include <unordered_set>
#include <cmath>
#include <cstring>
#include <random>

// https://github.com/ucb-bar/testchipip/blob/03535f56a6318236ab6abf5342d78eecf453984d/src/main/resources/testchipip/csrc/SimSerial.cc
#include <vpi_user.h>
#include <svdpi.h>

#define IS_WARMUP(cyc) (cyc - SIM_START) <= WARMUP_CYCLES
#define IS_MEASUREMENT(cyc) (cyc - SIM_START) > WARMUP_CYCLES && (cyc - SIM_START) <= WARMUP_CYCLES + MEASUREMENT_CYCLES
#define IS_DRAIN(cyc) (cyc - SIM_START) > WARMUP_CYCLES + MEASUREMENT_CYCLES

using namespace std;

/* Global Variables */
int WARMUP_CYCLES = 2000;
int MEASUREMENT_CYCLES = 3000;
unsigned long long NUM_FLITS
unsigned long long SIM_START = (unsigned long long) -1; // set to first cycle where NoC begins accepting traffic

/* plusarg name for the filepath corresponding to the traffic matrix. */
char* TRAFFIC_MATRIX_PLUSARG = "+TRAFFIC_MATRIX="

/* TRAFFIC_MATRIX[i][e] specifies the flow rate from ingress i to egress e. */
int** TRAFFIC_MATRIX

/* Number of packets received between each src->dst pair during the measurement phase.
 * Used to compute the throughput at the end of the simulation. */
long** PACKETS_RECVD_M;

unsigned long long NUM_FLITS;
unsigned long long NUM_INGRESSES;

/* C representation of a flit. */
struct flit {
  bool head;
  bool tail;
  unsigned int ingress_id; // src
  unsigned int egress_id; // dst
  unsigned long long payload; // cycle_time flit was created
};

/* Source Queues for input into NoC */
vector<vector<struct flit>>* SRC_QUEUES;

/* All packets currently in-flight in the NoC. */
unordered_set<flit>* FLITS_IN_FLIGHT;

/* Called at the beginning of simulation to initialize global state. */
extern "C" void instrumentationunit_init(
  unsigned long long num_ingresses,
  unsigned long long num_egresses
)
{
  // Initialize global state
  NUM_FLITS = 4; // TODO (ANIMESH): PARAMETERIZE THIS
  NUM_INGRESSES = num_ingresses;
  SRC_QUEUES = new vector<vector<flit>>(NUM_INGRESSES, vector<flit>());
  FLITS_IN_FLIGHT = new unordered_set<flit>();
  TRAFFIC_MATRIX = new *int[num_ingresses + 1];
  PACKETS_RECVD_M = new *long[num_ingresses + 1];
  for (int i = 0; i < num_ingresses; i++) {
    TRAFFIC_MATRIX[i] = new int[num_egresses + 1];
    PACKETS_RECVD_M[i] = new long[num_egresses + 1];
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
    abort();
  }
  FILE* traffic_matrix_file = fopen(filepath, "r")
  int num_iterations = 0;
  while (feof(traffic_matrix_file) != 0) {
    int ingress; int egress; int flow_rate;
    fscanf(file, " "); // skip leading whitespace
    int num_matches = fscanf(traffic_matrix_file, "%d %d %d", &ingress, &egress, &flow_rate);
    if (num_matches != 3) {
      printf("C++ Sim: failed to read traffic matrix file (incorrect format)\n")
      abort();
    }
    if (ingress >= NUM_INGRESSES || egress >= NUM_EGRESSES) {
      printf("C++ Sim: read invalid ingress or egress:\n\tIngress: %d with max %d\n\tEgress: %d with max %d", ingress, NUM_INGRESSES, egress, NUM_EGRESSES);
      abort();
    }
    TRAFFIC_MATRIX[ingress][egress] = flow_rate;
    num_iterations++;
  }
  if (num_iterations != NUM_EGRESSES * NUM_INGRESSES) {
    printf("C++ Sim: not enough entries in traffic file. Expected %d but got %d", NUM_EGRESSES * NUM_INGRESSES, num_iterations);
    abort();
  }
  fclose(traffic_matrix_file);
  for (int i = 0; i < NUM_INGRESSES; i++) {
    int ingress_routing = TRAFFIC_MATRIX[i];
    int sum = 0;
    for (int e = 0; e < NUM_INGRESSES; e++) {
      sum += ingress_routing[e];
    }
    if (sum != 100) {
      printf("C++ Sim: Invalid traffic matrix (each row must sum to 100)\n")
      abort();
    }
  }

  return;
}

/* Given an input ingress, returns the egressID of the egress to send a packet to
 * at this cycle. No packet should be sent if the output is -1, otherwise cast the output to
 * unsigned long long to get the egressID for the output
 */
long long gen_packet(
  unsigned long long ingress_id,
  unsigned long long cycle_count
) {
  int generate_sample = uniform_int_distribution(1, NUM_FLITS);
  if (generate_sample != 1) return -1; // TODO: prevents packets from being injected too often

  int random_sample = uniform_int_distribution(1, 100);
  int ingress_traffic_pattern = TRAFFIC_MATRIX[ingress_id];

  int sum_so_far = 0;
  for (int e = 0; e < NUM_INGRESSES; e++) {
    int rate_for_e = ingress_traffic_pattern[e];
    if (rate_for_e == 0) continue;
    sum_so_far += rate_for_e;
    if (sum_so_far >= random_sample) {
      return e;
    }
  }
  return -1;
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
  if (noc_ready != 0 && SIM_START == (unsigned long long) -1) {
    SIM_START = cycle_count;
  }

  unsigned long long dest = gen_packet(ingress_id, cycle_count);
  if (((long long) dest) != -1) {
    for (int i = 0; i < NUM_FLITS; i++) {
      flit* new_flit = new flit;
      new_flit->head = i == 0;
      new_flit->tail = i == (NUM_FLITS - 1);
      new_flit->ingress_id = ingress_id;
      new_flit->egress_id = dest;
      if (IS_MEASUREMENT(cycle_count)) {
        new_flit->payload = cycle_count;
        FLITS_IN_FLIGHT.append(new_flit);
      } else {
        new_flit->payload = (unsigned long long) -1;
      }
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
    struct flit head_flit = (*SRC_QUEUES)[ingress_id].at(0);
    if (head_flit.payload != (unsigned long long) -1) {
      // payload set to -1 during warmup/drain phases, don't track those flits
      FLIGHTS_IN_FLIGHT.insert((*SRC_QUEUES)[ingress_id].at(0));
    }
    (*SRC_QUEUES)[ingress_id].erase((*SRC_QUEUES)[ingress_id].begin());
  }

  return;
}

bool METRICS_PRINTED = FALSE;

extern "C" void egressunit_tick(
  unsigned long long egress_id,
  unsigned long long cycle_count,
  unsigned char noc_valid,
  unsigned char flit_in_head,
  unsigned char flit_in_tail,
  unsigned long long flit_in_ingress_id,
  unsigned long long flit_in_payload,

  unsigned char* egressunit_ready,
  unsigned char* success // set to 1 when all measurement flits have arrived
) {
  // always ready to receive packets
  *egressunit_ready = 1;

  if (noc_valid == 0) return;

  struct flit received_flit;
  received_flit.head = flit_in_head;
  received_flit.tail = filt_in_tail;
  received_flit.ingress_id = flit_in_ingress_id;
  received_flit.egress_id = egress_id;
  received_flit.payload = flit_in_payload;

  if (received_flit.payload != (unsigned long long) -1) {
    FLITS_IN_FLIGHT.erase(received_flit);
  }

  if (received_flit.tail != 0 && IS_MEASUREMENT(received_flit.payload)) {
    PACKETS_RECVD_M[index(received_flit.ingress_id, received_flit.egress_id)]++;
  }

  if (FLITS_IN_FLIGHT.size() != 0) return;
  *success = 1;

  /* Compute and print out metrics. (TODO) */
  if (!METRICS_PRINTED) {
    printf("C++ Sim: THROUGHPUT IS %f\n", compute_throughput());
    METRICS_PRINTED = true;
  }

  return;
}

double compute_throughput() {
  // expected traffic from ingress i to egress e is TRAFFIC_MATRIX[i][e] * MEASUREMENT_CYCLES / 100
  // actual traffic is PACKETS_RECVD_M
  double min_throughput = 1.0;
  for (int i = 0; i < num_ingresses; i++) {
    for (int e = 0; e < num_egresses; e++) {
      long expected_traffic = TRAFFIC_MATRIX[i][e] * MEASUREMENT_CYCLES / 100;
      long actual_traffic = PACKETS_RECVD_M[i][e];
      long channel_throughput = actual_traffic / expected_traffic
      if (channel_throughput > 1) {
        printf("C++ Sim: throughput for (ingress %d, egress %d) greater than 1: %f\n", i, e, channel_throughput);
      }
      min_throughput = (channel_throughput < min_throughput) ? channel_throughput : min_throughput;
    }
  }
  return min_throughput;
}