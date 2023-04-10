The Constellation NoC Generator
=======================================================

**Documentation**: [constellation.readthedocs.io](http://constellation.readthedocs.io).

Constellation is a Chisel NoC RTL generator framework designed to provide the core interconnect fabric for heterogeneous many-core, many-accelerator SoCs.

 - Constellation generates **packet-switched wormhole-routed networks with virtual networks and credit-based flow control**
 - Constellation supports **arbitrary directed graph network topologies**, including **irregular** and **hierarchical** network topologies
  - Constellation includes a **routing algorithm verifier and routing-table compiler**, which can verify and generate deadlock-free routing tables for arbitrary topologies
 - Constellation is a **protocol-independent transport layer**, yet is capable of compliant deadlock-free transport of protocols like **AXI-4** and **TileLink**
 - Constellation supports drop-in **integration in Chipyard/Rocketchip SoCs**
 - Constellation is **rigorously tested**, with almost 100 different tests across as many network configurations

![Constellation SoC](docs/source/diagrams/bigsoc.svg?raw=true)

Publications
--------------

[Constellation: An Open-Source SoC-Capable NoC Generator](https://ieeexplore.ieee.org/abstract/document/9911299/)
```
@inproceedings{zhao2022constellation,
  title={Constellation: An Open-Source SoC-Capable NoC Generator},
  author={Zhao, Jerry and Agrawal, Animesh and Nikolic, Borivoje and Asanovi{\'c}, Krste},
  booktitle={2022 15th IEEE/ACM International Workshop on Network on Chip Architectures (NoCArc)},
  pages={1--7},
  year={2022},
  organization={IEEE}
}
```

Acknowledgements
------------------
Research was partially funded by SLICE Lab industrial sponsors and affiliates Amazon, Apple, Google, Intel, Qualcomm, and Western Digital. The views and opinions of authors expressed herein do not necessarily state or reflect those of the United States Government or any agency thereof.
