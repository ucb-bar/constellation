#include <cstdio>
#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include <cmath>
#include <cstring>
#include <random>
#include <chrono>

// https://github.com/ucb-bar/testchipip/blob/03535f56a6318236ab6abf5342d78eecf453984d/src/main/resources/testchipip/csrc/SimSerial.cc
#include <vpi_user.h>
#include <svdpi.h>

using namespace std;

/* Number of cycles spent warming up and measuring performance of the network. */
int WARMUP_CYCLES;
int MEASUREMENT_CYCLES;

/* Maximum cycles spent in drain phase. */
int DRAIN_TIMEOUT;

/* Cycle the simulation begins. */
unsigned long long SIM_START = (unsigned long long) -1;

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
  unsigned long long ingress_id; // src
  unsigned long long egress_id; // dst
  unsigned long long payload; // cycle_time flit was created
} flit;

/* Source Queues for input into NoC */
vector<queue<struct flit*>>* SRC_QUEUES;

/* All packets currently in-flight in the NoC. keys: flit unique id, values: destination egress id */
map<long, unsigned long long>* FLITS_IN_FLIGHT;

// todo: Document
char* last_flit_out_valid;

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

long _UNIQ_ID = 0;
long gen_unique_id_flit() {
  _UNIQ_ID = _UNIQ_ID + 1;
  return _UNIQ_ID;
}

/* Generates the payload for a measurement flit. */
unsigned long long gen_flit_payload(unsigned long long cycle_count) {
  return (gen_unique_id_flit() << 32) | (0xFFFFFFFF & cycle_count);
}

/* Given a flit's payload, returns the flit's unique id (use only on measurement flits). */
unsigned long get_unique_id_flit(unsigned long long payload) {
  return (payload >> 32);
}

/* Given a flit's payload, returns the cycle the flit was generated (use only on measurement flits). */
unsigned long get_cyc_flit(unsigned long long payload) {
  return payload & 0xFFFFFFFF;
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
  FLITS_IN_FLIGHT = new map<long, unsigned long long>();
  last_flit_out_valid = new char[NUM_INGRESSES];
  for (int i = 0; i < NUM_INGRESSES; i++) {
    last_flit_out_valid[i] = 0;
  }

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
  if (traffic_matrix_file == NULL) {
    printf("C++ Sim: Unable to open traffic matrix file\n");
    abort();
  }
  fscanf(traffic_matrix_file, " "); // skip leading whitespace
  int num_matches = fscanf(traffic_matrix_file, "%d %d %d %d", &NUM_FLITS, &WARMUP_CYCLES, &MEASUREMENT_CYCLES, &DRAIN_TIMEOUT);
  if (num_matches != 4) {
      printf("C++ Sim: failed to read config options from traffic matrix file. Read only %d args.\n", num_matches);
      abort();
  }

  int num_iterations = 0;
  while (feof(traffic_matrix_file) == 0) {
    int ingress; int egress; float flow_rate;
    fscanf(traffic_matrix_file, " "); // skip leading whitespace
    num_matches = fscanf(traffic_matrix_file, "%d %d %f", &ingress, &egress, &flow_rate);
    if (num_matches != 3) {
      printf("C++ Sim: failed to read traffic matrix (incorrect format). Read only %d args.\n", num_matches);
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
      sum += ingress_routing[e];
    }
    if (sum > 1.0) {
      printf("C++ Sim: Invalid traffic matrix (each row must sum to <= 1.0)\n");
      abort();
    }
  }

  printf("DEBUG: C++ sim initialized\n");

  return;
}

// TODO: MAKE SEED PARAMETERIZABLE (OR REQUEST RANDOM SEED)
unsigned int seed = 152; // std::chrono::system_clock::now().time_since_epoch().count();
std::default_random_engine generator (seed);
uniform_real_distribution<float> egress_selector(0.0, 1.0);

/* Given an input ingress, returns the egressID of the egress to send a packet to
 * at this cycle. No packet should be sent if the output is -1, otherwise cast the output to
 * unsigned long long to get the egressID for the output
 */
long long gen_packet(
  unsigned long long ingress_id,
  unsigned long long cycle_count
) {

  float selected_ingress_sample = egress_selector(generator);

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
      if (IS_MEASUREMENT(cycle_count)) {
        new_flit->payload = gen_flit_payload(cycle_count);
      } else {
        new_flit->payload = (unsigned long long) -1;
      }
      (*SRC_QUEUES)[ingress_id].push(new_flit);
    }
  }

  // TODO (ANIMESH):
  if (noc_ready != 0 && last_flit_out_valid[ingress_id] != 0) {
    if ((*SRC_QUEUES)[ingress_id].size() == 0) {
      printf("C++ Sim: Tried to pop from an empty source queue.\n");
      abort();
    }
    flit* last_flit = (*SRC_QUEUES)[ingress_id].front();
    (*SRC_QUEUES)[ingress_id].pop();

    if (last_flit->payload != (unsigned long long) -1) {
      (*FLITS_IN_FLIGHT).insert(pair<long, unsigned long long>(get_unique_id_flit(last_flit->payload), last_flit->egress_id));
      printf("DEBUG: Inserting flit %ld to egress %llu at cycle %llu\n", get_unique_id_flit(last_flit->payload), last_flit->egress_id, cycle_count);
      printf("\tFLITS_IN_FLIGHT now size %d\n", (*FLITS_IN_FLIGHT).size());
    } else {
      // printf("DEBUG: Inserting flit W to egress %llu at cycle %llu\n", next_flit->egress_id, cycle_count);
    }
    delete last_flit; // no need to track warmup/drain flits
  }

  bool flit_available = (*SRC_QUEUES)[ingress_id].size() > 0;

  // TODO (ANIMESH): THE VALUES HERE ARE SENT OUT THE CYCLE AFTER THIS ONE.
  // What the logic above does: If we outputted a valid signal last cycle, its visible this cycle.
  // If we also see a ready cycle this signal, that means both ready and valid were high so we should
  // remove a flit from the queue
  flit* next_flit = NULL;
  if (flit_available) {
    next_flit = (*SRC_QUEUES)[ingress_id].front();
    *flit_out_head = next_flit->head;
    *flit_out_tail = next_flit->tail;
    *flit_out_egress_id = next_flit->egress_id;
    *flit_out_payload = next_flit->payload;

    *flit_out_valid = 1;
  } else {
    *flit_out_valid = 0;
  }

  last_flit_out_valid[ingress_id] = (flit_available) ? 1 : 0;

  return;
}

bool METRICS_PRINTED = false;

double compute_throughput() {
  // expected traffic from ingress i to egress e is TRAFFIC_MATRIX[i][e] * MEASUREMENT_CYCLES
  // actual traffic is PACKETS_RECVD_M
  FILE* csv = fopen("noc-flow-throughputs.csv", "w");
  fprintf(csv, "ingress,egress,throughput\n");
  double min_throughput = 1.0;
  for (int i = 0; i < NUM_INGRESSES; i++) {
    for (int e = 0; e < NUM_EGRESSES; e++) {
      long expected_traffic = static_cast<long>(TRAFFIC_MATRIX[i][e] * MEASUREMENT_CYCLES);
      long actual_traffic = PACKETS_RECVD_M[i][e];
      double channel_throughput = ((double) actual_traffic / (double) expected_traffic);
      fprintf(csv, "%d,%d,%f\n", i, e, channel_throughput);
      min_throughput = (channel_throughput < min_throughput) ? channel_throughput : min_throughput;
    }
  }
  fclose(csv);
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

  if (IS_DRAIN_TIMEOUT(cycle_count) && !METRICS_PRINTED) {
    printf("C++ Sim: Ending simulation at cycle %llu\n", cycle_count);
    printf("C++ Sim: Simulation timed out (drain took too long). Ending so you can view a waveform.\n", cycle_count);
    printf("C++ Sim: THROUGHPUT IS %f\n", compute_throughput());
    METRICS_PRINTED = true;
    *success = 1;
    return;
  }

  if (noc_valid == 0) return;

  struct flit received_flit;
  received_flit.head = flit_in_head;
  received_flit.tail = flit_in_tail;
  received_flit.ingress_id = flit_in_ingress_id;
  received_flit.egress_id = egress_id;
  received_flit.payload = flit_in_payload;

  if (received_flit.payload != (unsigned long long) -1) {
    int num_erased = (*FLITS_IN_FLIGHT).erase(get_unique_id_flit(received_flit.payload));
    printf("DEBUG: egress %llu received flit %ld at cycle %llu\n", egress_id, get_unique_id_flit(received_flit.payload), cycle_count);
    printf("\tDEBUG: erased %d from set. %d left.\n", num_erased, (*FLITS_IN_FLIGHT).size());
  } else {
    // printf("DEBUG: egress %llu received W at cycle %llu\n", egress_id, cycle_count);
  }

  if (received_flit.tail != 0 && received_flit.payload != (unsigned long long) -1 && IS_MEASUREMENT(cycle_count)) {
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