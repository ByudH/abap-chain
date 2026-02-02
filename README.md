# 🔗 ABAPChain

Graph-based Agent Framework for ABAP

## 💡 Overview

ABAPChain is an MVP framework designed to bring graph-based application development to the ABAP stack. Inspired by LangGraph, it allows developers to define stateful, agentic workflows using nodes and edges directly within SAP.

## 🚀 Key Features

- **Graph Architecture**: Define cyclic flows and state transitions.
- **Agent Support**: Orchestrate autonomous steps for LLM integration.
- **Pure ABAP**: Designed to run natively on the ABAP Application Server.

## 📦 Installation & Prerequisites

### Prerequisites

To use ABAPChain, you need access to an ABAP Environment (e.g., SAP BTP ABAP Environment, SAP S/4HANA Public Cloud, or an on-premise system with recent ABAP capabilities).

The following authorization object is needed to read the bali log

**S_APPL_LOG** with:

- **ALG_OBJECT** = application log object
- **ALG_SUBOBJ** = subobject
- **ACTVT** = 03

I currently use:

- **ALG_OBJECT**: ZABAPCHAIN
- **ALG_SUBOBJ**: AGENT_RUN

### Installation

You can install this project by cloning the repository using **abapGit**.

1. Install [abapGit](https://abapgit.org/) on your ABAP system.
2. Create a new offline or online repository.
3. Clone this repository URL into your system.
4. Pull the objects.

## 🛠️ Usage Example

This section demonstrates how to build and run a simple agent, such as a “Weather Bot”.

The following example:
1. Sets debug mode.
2. Instantiates a tool (`zcl_ai_tool_weather_info`).
3. Uses `zcl_ai_agent_builder` to create a builder.
4. Adds an LLM node (`zcl_ai_node_llm`) and a Tool node (`zcl_ai_node_tool`).
5. Sets a user prompt: ‘Tell me the current weather in Munich, Germany.’
6. Connects the nodes:
* LLM -> Tool (on control value ‘TOOL’)
* Tool -> LLM (always)
7. Builds and runs the agent, then outputs the result.

```abap
METHOD run_weather_agent.
  " 1. Enable Debug Mode (Optional)
  " Set to true if you want to see logged steps in the console
  zcl_abapchain_logger=>set_debug_mode( debug = abap_true out = out ).

  " 2. Instantiate Tools and Nodes
  DATA(weather_tool) = NEW zcl_ai_tool_weather_info( ).
  DATA(builder)      = zcl_ai_agent_builder=>new( ).
  DATA(llm_node)     = NEW zcl_ai_node_llm( ).
  DATA(tool_node)    = NEW zcl_ai_node_tool( ).

  " 3. Configure Prompt
  llm_node->set_user_prompt( 'Tell me the current weather in Munich, Germany.' ).

  " 4. Build Graph and Connect Nodes
  builder->add_node( llm_node )->add_tool( weather_tool
  )->add_node( tool_node
  )->connect_on_control(
      from_node_id  = llm_node->node_id
      to_node_id    = tool_node->node_id
      control_value = 'TOOL'
  )->connect_always(
      from_node_id = tool_node->node_id
      to_node_id   = llm_node->node_id
  ).

  " 5. Run Agent
  DATA(agent)       = builder->build( ).
  DATA(final_state) = agent->run( ).

  " 6. Output Results
  out->write( zcl_ai_utils=>messages_to_string( final_state-messages ) ).
ENDMETHOD.
```
## 🧠 LLM-Driven Planner Node (Optional)

ABAPChain optionally supports LLM-driven planning through a dedicated planner node.

Instead of hard-coding all routing logic in ABAP, a planner node can decide which branch of the graph to execute next based on the current state, messages, and context. This enables dynamic workflows such as conditional routing, tool gating, and human-in-the-loop approvals.

The planner produces a structured decision (for example a branch label), which the orchestrator uses to continue execution.

``àbap
DATA(planner_node) = NEW zcl_ai_node_llm_planner( ).

builder
  ->add_node( planner_node )
  ->connect_on_branch(
       from_node_id = planner_node->node_id
       branch_label = 'TOOL'
       to_node_id   = tool_node->node_id
   ).
```
Using a llm planner node is optional.

## 🔌 Extensibility Guide

ABAPChain is designed to be extensible. Developers can create custom nodes and tools to fit their specific business logic.

### 1. Adding Custom Nodes

To add a custom node, you must create a class that implements the `zif_ai_node` interface.

**Recommendation:**
We recommend inheriting from `zcl_ai_node_base` as it automatically handles shared functionalities like logging and error handling.

**Requirements:**
* **Interface**: Implement `zif_ai_node` (or inherit from `zcl_ai_node_base`).
* **Execution Logic**: Redefine the `do_execute` method to implement the node’s core logic.
* **Persistence**: Redefine `zif_ai_node~get_configuration` and `zif_ai_node~set_configuration` to handle saving and loading the node’s state.

### 2. Adding Custom Tools

To add a custom tool (like the weather tool in the example), create a class that implements the `zif_ai_tool` interface.

**Recommendation:**
We recommend inheriting from `zcl_ai_tool_base`, which provides helper methods and standard behavior.

**Requirements:**
* **Interface**: Implement `zif_ai_tool` (or inherit from `zcl_ai_tool_base`).
* **Schema Definition**: Redefine `define_argument_metadata` to specify the tool’s input schema using `zcl_tool_schema` types.
* **Execution Logic**: Redefine `do_execute` to implement the tool’s actual functionality.
* **Persistence**: Redefine `zif_ai_tool~get_configuration` and `zif_ai_tool~set_configuration` to ensure the tool’s state can be persisted.

### 3. Contributing to the Framework

We welcome contributions! If you want to improve the framework:
1. **Fork** the repository on GitHub.
2. Create a new **feature branch** for your changes.
3. Commit your changes.
4. Submit a **Pull Request (PR)** to the main repository.

## 📜 License

This project is licensed under the **MIT License**.

### ⚠️ Disclaimer

**This is a university project and is provided “as-is”.**
There is no warranty for the quality, reliability, or accuracy of the software. Usage is at your own risk. We do not provide official support or guarantee maintenance.
