Setting up Constellation
==================================

Constellation can be installed and used in three modes.

 * **Minimal-Standalone** operation requires a minimal installation, and enables evaluation of NoCs within simple harnesses and evaluation frameworks.
 * **Chiypard-Standalone** runs Constellation as a Chipyard "subproject", enabling more detailed evaluation of standalone NoCs.
 * **Chipyard-SoC** generates a complete Chipyard SoC with a Constellation NoC

A table below summarizes the differences. For most users, **Chipyard-Standalone** and **Chipyard-SoC** are the most useful operation modes. **Minimal-Standalone** should only be used in environments prohibitive to a full Chipyard installation.
   
.. list-table::
   :widths: 40 25 50
   :header-rows: 1

   * - Mode
     - Requires
     - Capabilities
   * - **Minimal-Standalone**
     - - Espresso
       - Rocketchip
     - - Standalone NoC generation
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


Installing Rocketchip
---------------------

.. Note:: Rocketchip should only be manually installed for the **Minimal-standalone** operation mode.

For a **Minimal-Standalone** installation, Rocketchip must be manually installed as a locally-published Scala project.


.. code-block:: shell

   git clone https://github.com/chipsalliance/rocket-chip.git
   cd rocket-chip
   git checkout 045d03c54bce7636401b1b4b17d6a3677356dfe0
   git submodule update --init --recursive
   sbt "publishLocal"
   sbt "project api-config-chipsalliance; set publishArtifact := true; publishLocal"
   sbt "project rocket-macros; set publishArtifact := true; publishLocal"
   sbt "project hardfloat; set publishArtifact := true; publishLocal"

Installing Espresso
-------------------

`Espresso <https://en.wikipedia.org/wiki/Espresso_heuristic_logic_minimizer>`_ is a logic minimizer tool. Constellation uses Espresso to generate efficient routing decode tables.

To use Constellation, ``espresso`` should be on your ``PATH``.

.. code-block:: shell

   git clone https://github.com/chipsalliance/espresso.git
   cd espresso
   mkdir -p build
   cd build
   cmake ../ -DBUILD_DOC=OFF -DCMAKE_INSTALL_PREFIX=/path/to/install/bin
   make install

Installing Minimal-Standalone
-----------------------------

To use Constellation in **Minimal-Standalone** mode, it is sufficient to clone the repository after installing Rocketchip and Espresso.

.. code-block:: shell

   git clone https://github.com/ucb-bar/constellation.git

Installing with Chipyard
------------------------

To use Constellation in **Chipyard-Standalone** or **Chipyard-SoC** mode, follow the instructions for installing Chipyard `here <https://chipyard.readthedocs.io>`_. You must use Chipyard 1.8 or later.

After following those steps, run the following

.. code-block:: shell

   make -C generators/constellation/src/main/resources/csrc/netrace netrace.o CFLAGS="-fPIC -O3"
