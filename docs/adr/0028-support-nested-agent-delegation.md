# Support nested agent delegation

Status: accepted

Mevedel will support nested delegation at every position in the agent tree rather than reserving spawning for the root session or a coordinator. An agent may create children when its resolved tool set includes `Agent`, allowing specialized roles and future swarm workflows to choose delegation capability without a separate orchestration class. The spawn tree owns canonical paths and automatic result delivery, while explicit communication may address any agent in the root session tree.
