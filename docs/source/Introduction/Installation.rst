Setting up Constellation
==================================

Constellation can be installed and used in two modes.

 * **Chiypard-Standalone** runs Constellation as a Chipyard "subproject", enabling more detailed evaluation of standalone NoCs.
 * **Chipyard-SoC** generates a complete Chipyard SoC with a Constellation NoC

A table below summarizes the differences. For most users, **Chipyard-Standalone** and **Chipyard-SoC** are the most useful operation modes.

.. list-table::
   :widths: 40 25 50
   :header-rows: 1

   * - Mode
     - Requires
     - Capabilities
   * - **CY-Standalone**
     - - Espresso
       - Chipyard
     - - Standalone NoC generation
       - Waveform generation
       - NoC visualization
   * - **CY-SoC**
     - - Espresso
       - Chipyard
     - - NoC integration with SoC



Installing with Chipyard
------------------------

To use Constellation in **Chipyard-Standalone** or **Chipyard-SoC** mode, follow the instructions for installing Chipyard `here <https://chipyard.readthedocs.io>`_. You must use the most recent Chipyard version

After following those steps, run the following

.. code-block:: shell

   make -C generators/constellation/src/main/resources/csrc/netrace netrace.o CFLAGS="-fPIC -O3"
