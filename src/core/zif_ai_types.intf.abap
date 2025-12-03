INTERFACE zif_ai_types
  PUBLIC .
  "Shared types for node_id and agent_id
  types ty_node_id type UUID.
  types ty_agent_id type UUID. "Could be UUID
  " TODO: Add custom types for ZCL_AI_GRAPH_STATE class
  " Add Structure for the state
  types: begin of ty_graph_state,
    messages        type string,  "LLM responses / narrative"
    last_tool_name  type string,  "debug info"
    branch_label    type string,  "routing: ON_LABEL"
    result_json     type string,  "final output"
  end of ty_graph_state.

ENDINTERFACE.
