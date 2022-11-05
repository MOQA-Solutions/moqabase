
# Introduction

**Moqabase** is an **Erlang Backend Server** based on **Islands** principle, it was designed
to be **Highly Scalable** and **Fault Tolerant** face to **Concurrency**, it can be used as **Backend Server** in **Any**
Erlang **Real Time**, **Concurrent**, **Distributed** and **Scalable** Application, that supports *3-tiers*
Architecture(**Client -> Frontend -> Backend**).<br>

# Installation

- First, you should configure your **Cluster** as a set of **Islands** (at least 1 Island)as your needs,
for more informations about **Islands** you can check [Moqabase Guide](https://github.com/MOQA-Solutions/moqabase/blob/master/docs/moqabase_guide.asciidoc).<br>
- Next, you should **edit** `moqabase:partition_to_node/1` to **adapt** your **Cluster**, it is configured
by default to support just 1 Island(2 Nodes).
  - **Partitions** are `atoms` corresponding to `numbers`
  - They start from `'1'`
  - You can't create the Partition `'N'` if `'N-1'` does not exist
  - So if you have 1 **Island**, then you will have 2 **Partitions** in **each** of your **2 Nodes** :
    - Partition `'1'` which is **Primary Partition** for the **First** Node and **Secondary Partition** for the **Second** Node
    - Partition `'2'` which is **Primary Partition** for the **Second** Node and **Secondary Partition** for the **First** Node.
  - `moqabase:partition_to_node(Partition)` returns the **Node** that have `Partition` as its **Primary Partition**
  - If you have more Islands you should **update** this function to support **Partition** `'3'` , `'4'` , ....
- You should **edit** `moqabase_partition.hrl` to customize your **Database's Fields**
- Next you should **edit** `moqabase.app` for 2 reasons :
  - To **edit Start Argument** which represents the **Primary Partition** for **This Node**
    - As I said you **can't** use Partition `'N'` if you have not yet used `'N-1'`
  - To **edit**  your list of **Frontend Nodes** and to **change**(if necessary) the **Number of Procs** corresponding to 1 Fragment of `mnesia` Table.
- After that, you can start `moqabase` Application for **Each Node** of your Cluster **STARTING** from the First
Node with Partition `'1'` to the **N**th Node with Partition `'N'` 
```
rebar3 shell --sname somenode@somehost
```




