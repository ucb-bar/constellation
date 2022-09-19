Performance Evaluation
==================================

Installing Chipyard
---------------------
The performance evaluation framework runs best inside Chipyard, a platform for designing and simulating SoCs. Follow Chipyard's `Setup <https://chipyard.readthedocs.io/en/stable/Chipyard-Basics/Initial-Repo-Setup.html>`_ instructions to install and initialize Chipyard.

Once chipyard is installed, check out the saturn-v branch of chipyard and update the constellation submodule to the latest version

.. code-block:: shell

   cd chipyard
   git checkout saturnv
   cd generators/constellation
   git pull constellation

Initializing Constellation
-------------------

In addition to the Constellation Installation instructions, we also have to initialize parts of the constellation repository used for performance evaluation:

.. code-block:: shell
    # Initialize netrace
    cd chipyard/generators/constellation
    cd src/main/resources
    git submodule update --init csrc/netrace
    cd netrace
    make netrace.o


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

   make -C generators/constellation/src/main/resources/csrc/netrace
