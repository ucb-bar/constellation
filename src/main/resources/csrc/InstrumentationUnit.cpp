#include <cstdio>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <cmath>
#include <cstring>
#include <random>
#include <chrono>

// https://github.com/ucb-bar/testchipip/blob/03535f56a6318236ab6abf5342d78eecf453984d/src/main/resources/testchipip/csrc/SimSerial.cc
#include <vpi_user.h>
#include <svdpi.h>

using namespace std;

/* Global Variables */
int WARMUP_CYCLES = 200;
int MEASUREMENT_CYCLES = 300;
int DRAIN_TIMEOUT = 50;
unsigned long long SIM_START = (unsigned long long) -1; // set to first cycle where NoC begins accepting traffic

/* plusarg name for the filepath corresponding to the traffic matrix. */
const char* TRAFFIC_MATRIX_PLUSARG = "+TRAFFIC_MATRIX=";

/* TRAFFIC_MATRIX[i][e] specifies the flow rate from ingress i to egress e. */
float** TRAFFIC_MATRIX;

/* Number of packets received between each src->dst pair during the measurement phase.
 * Used to compute the throughput at the end of the simulation. */
long** PACKETS_RECVD_M;

/* Number of flits per packet. */
unsigned long long NUM_FLITS;

/* Number of ingresses and egresses in the network. */
unsigned long long NUM_INGRESSES;
unsigned long long NUM_EGRESSES;

/* C representation of a flit. */
typedef struct flit {
  bool head;
  bool tail;
  unsigned int ingress_id; // src
  unsigned int egress_id; // dst
  unsigned long long payload; // cycle_time flit was created
  unsigned int body_id; // flit index in packet. Used only in C++ model to compare flits
} flit;

struct flit_comparator {
  /* Returns true if lflit < rflit, used by FLITS_IN_FLIGHT set. */
  bool operator() (const flit* lflit, const flit* rflit) {
    long long lflit_sum = (long long) (lflit->ingress_id + lflit->egress_id + lflit->payload + lflit->body_id);
    long long rflit_sum = (long long) (rflit->ingress_id + rflit->egress_id + rflit->payload + rflit->body_id);
    return lflit_sum < rflit_sum;
  }
};

/* Source Queues for input into NoC */
vector<queue<struct flit*>>* SRC_QUEUES;

/* All packets currently in-flight in the NoC. */
set<flit*, flit_comparator>* FLITS_IN_FLIGHT;

/* Returns true if cycle cycle_count is in the warmup phase. */
bool IS_WARMUP(unsigned long long cycle_count) {
  if (SIM_START < 0) {
    printf("C++ Sim: SIM_START not set\n");
    abort();
  }
  unsigned long long adjusted_cnt = cycle_count - SIM_START;
  return adjusted_cnt <= WARMUP_CYCLES;
}

/* Returns true if cycle cycle_count is in the measurement phase. */
bool IS_MEASUREMENT(unsigned long long cycle_count) {
  if (SIM_START < 0) {
    printf("C++ Sim: SIM_START not set\n");
    abort();
  }
  unsigned long long adjusted_cnt = cycle_count - SIM_START;
  return (adjusted_cnt > WARMUP_CYCLES) && (adjusted_cnt <= (WARMUP_CYCLES + MEASUREMENT_CYCLES));
}

/* Returns true if cycle cycle_count is in the drain phase. */
bool IS_DRAIN(unsigned long long cycle_count) {
  if (SIM_START < 0) {
    printf("C++ Sim: SIM_START not set\n");
    abort();
  }
  unsigned long long adjusted_cnt = cycle_count - SIM_START;
  return adjusted_cnt > (WARMUP_CYCLES + MEASUREMENT_CYCLES);
}

/* Returns true if cycle cycle_count has exceeded the maximum time allowed for the drain phase. */
bool IS_DRAIN_TIMEOUT(unsigned long long cycle_count) {
  if (SIM_START < 0) {
    printf("C++ Sim: SIM_START not set\n");
    abort();
  }
  unsigned long long adjusted_cnt = cycle_count - SIM_START;
  return adjusted_cnt > (WARMUP_CYCLES + MEASUREMENT_CYCLES + DRAIN_TIMEOUT);
}

/* Called at the beginning of simulation to initialize global state. */
extern "C" void instrumentationunit_init(
  unsigned long long num_ingresses,
  unsigned long long num_egresses
) {
  // Initialize global state
  NUM_INGRESSES = num_ingresses;
  NUM_EGRESSES = num_egresses;
  SRC_QUEUES = new vector<queue<flit*>>(NUM_INGRESSES, queue<flit*>());
  FLITS_IN_FLIGHT = new set<flit*, flit_comparator>();
  TRAFFIC_MATRIX = new float*[num_ingresses + 1];
  PACKETS_RECVD_M = new long*[num_ingresses + 1];
  for (int i = 0; i < num_ingresses; i++) {
    TRAFFIC_MATRIX[i] = new float[num_egresses + 1];
    PACKETS_RECVD_M[i] = new long[num_egresses + 1];
    for (int idx = 0; idx < num_egresses + 1; idx++) {
      TRAFFIC_MATRIX[i][idx] = 0.0;
      PACKETS_RECVD_M[i][idx] = (long) 0;
    }
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
    if (strncmp(input_arg, TRAFFIC_MATRIX_PLUSARG, strlen(TRAFFIC_MATRIX_PLUSARG)) == 0) {
      filepath = input_arg + strlen(TRAFFIC_MATRIX_PLUSARG);
      printf("C++ Sim: traffic matrix filepath is %s\n", filepath);
      break;
    }
  }
  if (filepath == NULL) {
    printf("C++ Sim: Unable to find traffic matrix filepath plusarg\n");
    abort();
  }
  FILE* traffic_matrix_file = fopen(filepath, "r");
  fscanf(traffic_matrix_file, " "); // skip leading whitespace
  fscanf(traffic_matrix_file, "%d", &NUM_FLITS);

  int num_iterations = 0;
  while (feof(traffic_matrix_file) == 0) {
    int ingress; int egress; float flow_rate;
    fscanf(traffic_matrix_file, " "); // skip leading whitespace
    int num_matches = fscanf(traffic_matrix_file, "%d %d %f", &ingress, &egress, &flow_rate);
    if (num_matches != 3) {
      printf("C++ Sim: failed to read traffic matrix file (incorrect format)\n");
      abort();
    }
    if (ingress >= NUM_INGRESSES || egress >= NUM_EGRESSES) {
      printf("C++ Sim: read invalid ingress or egress:\n\tIngress: %d with max %d\n\tEgress: %d with max %d\n", ingress, NUM_INGRESSES, egress, NUM_EGRESSES);
      abort();
    }
    TRAFFIC_MATRIX[ingress][egress] = flow_rate;
    num_iterations++;
  }
  if (num_iterations != NUM_EGRESSES * NUM_INGRESSES) {
    printf("C++ Sim: not enough entries in traffic file. Expected %d but got %d\n", NUM_EGRESSES * NUM_INGRESSES, num_iterations);
    abort();
  }
  fclose(traffic_matrix_file);
  for (int i = 0; i < NUM_INGRESSES; i++) {
    float* ingress_routing = TRAFFIC_MATRIX[i];
    float sum = 0;
    for (int e = 0; e < NUM_INGRESSES; e++) {
      // printf("DEBUG: i(%d) to e(%d): %f\n", i, e, TRAFFIC_MATRIX[i][e]);
      sum += ingress_routing[e];
    }
    if (sum > 1.0) {
      printf("C++ Sim: Invalid traffic matrix (each row must sum to <= 1.0)\n");
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
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::default_random_engine generator (seed);

  uniform_real_distribution<float> egress_selector(0.0, 1.0);
  float selected_ingress_sample = egress_selector(generator);
  // printf("\tDEBUG: pgen rand is %f\n", selected_ingress_sample);

  float* ingress_traffic_pattern = TRAFFIC_MATRIX[ingress_id];

  float sum_so_far = 0;
  for (int e = 0; e < NUM_INGRESSES; e++) {
    float rate_for_e = ingress_traffic_pattern[e];
    if (rate_for_e == 0) continue;
    sum_so_far += rate_for_e;
    if (sum_so_far >= selected_ingress_sample) {
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
    printf("C++ Sim: Simulation starting at cycle %llu\n", cycle_count);
    SIM_START = cycle_count;
  }

  // printf("C++ Sim DEBUG: ingressunit %d at cycle %llu\n", ingress_id, cycle_count);

  unsigned long long dest = gen_packet(ingress_id, cycle_count);
  if (((long long) dest) != -1) {
    for (int i = 0; i < NUM_FLITS; i++) {
      flit* new_flit = new flit;
      new_flit->head = i == 0;
      new_flit->tail = i == (NUM_FLITS - 1);
      new_flit->ingress_id = ingress_id;
      new_flit->egress_id = dest;
      new_flit->body_id = i;
      if (IS_MEASUREMENT(cycle_count)) {
        new_flit->payload = cycle_count;
      } else {
        new_flit->payload = (unsigned long long) -1;
      }
      (*SRC_QUEUES)[ingress_id].push(new_flit);
    }
    // printf("\tC++ Sim DEBUG: Generated packet\n");
  }

  bool flit_available = (*SRC_QUEUES)[ingress_id].size() > 0;
  if (flit_available) {
    *flit_out_valid = 1;
  } else {
    *flit_out_valid = 0;
  }

  // printf("\tDEBUG: noc ready is %d\n\tflit available is %d\n", noc_ready, flit_available);
  if (noc_ready != 0 && flit_available) {
    flit* next_flit = (*SRC_QUEUES)[ingress_id].front();
    (*SRC_QUEUES)[ingress_id].pop();

    *flit_out_head = next_flit->head;
    *flit_out_tail = next_flit->tail;
    *flit_out_egress_id = next_flit->egress_id;
    *flit_out_payload = next_flit->payload;
    if (next_flit->payload != (unsigned long long) -1) {
      // printf("\tC++ SIM DEBUG: INJECTING PACKET WITH PAYLOAD %llu\n", next_flit->payload);
      (*FLITS_IN_FLIGHT).insert(next_flit);
    } else {
      delete next_flit; // no need to track warmup/drain flits
    }
    // printf("\tC++ Sim DEBUG: NoC took packet\n");
  }

  return;
}

bool METRICS_PRINTED = false;

double compute_throughput() {
  // expected traffic from ingress i to egress e is TRAFFIC_MATRIX[i][e] * MEASUREMENT_CYCLES
  // actual traffic is PACKETS_RECVD_M
  double min_throughput = 1.0;
  for (int i = 0; i < NUM_INGRESSES; i++) {
    for (int e = 0; e < NUM_EGRESSES; e++) {
      long expected_traffic = static_cast<long>(TRAFFIC_MATRIX[i][e] * MEASUREMENT_CYCLES);
      long actual_traffic = PACKETS_RECVD_M[i][e];
      double channel_throughput = ((double) actual_traffic / (double) expected_traffic);
      if (channel_throughput > 1.0) {
        printf("C++ Sim: throughput for (ingress %d, egress %d) greater than 1: %f\n", i, e, channel_throughput);
        printf("\texpected traffic: %ld\n\tactual traffic: %ld\n", expected_traffic, actual_traffic);
        abort();
      }
      min_throughput = (channel_throughput < min_throughput) ? channel_throughput : min_throughput;
    }
  }
  return min_throughput;
}

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

  if (IS_DRAIN_TIMEOUT(cycle_count)) {
    printf("C++ Sim: Ending simulation at cycle %llu\n", cycle_count);
    printf("C++ Sim: Simulation timed out (drain took too long). Ending so you can view a waveform.\n", cycle_count);
    printf("C++ Sim: THROUGHPUT IS %f\n", compute_throughput());
    *success = 1;
    return;
  }

  if (noc_valid == 0) return;
  // printf("C++ Sim DEBUG: Egress %d received flit from NoC\n", egress_id);

  struct flit received_flit;
  received_flit.head = flit_in_head;
  received_flit.tail = flit_in_tail;
  received_flit.ingress_id = flit_in_ingress_id;
  received_flit.egress_id = egress_id;
  received_flit.payload = flit_in_payload;
  // printf("\tDEBUG: payload was %lld\n", (long long) flit_in_payload);

  if (received_flit.payload != (unsigned long long) -1) {
    int num_erased = (*FLITS_IN_FLIGHT).erase(&received_flit); // TODO: NOTHING ACTUALLY GETTING ERASED
    // printf("\tDEBUG: erased %d from set\n", num_erased);
  }

  if (received_flit.tail != 0 && IS_MEASUREMENT(received_flit.payload)) {
    // printf("\tC++ SIM DEBUG: received tail for flit with payload %llu\n", received_flit.payload);
    PACKETS_RECVD_M[received_flit.ingress_id][received_flit.egress_id]++;
  }

  if ((*FLITS_IN_FLIGHT).size() != 0 || !IS_DRAIN(cycle_count)) {
    *success = 0;
    return;
  }

  *success = 1;

  /* Compute and print out metrics. (TODO: LATENCY) */
  if (!METRICS_PRINTED) {
    printf("C++ Sim: Ending simulation at cycle %llu\n", cycle_count);
    printf("C++ Sim: THROUGHPUT IS %f\n", compute_throughput());
    METRICS_PRINTED = true;
  }

  return;
}