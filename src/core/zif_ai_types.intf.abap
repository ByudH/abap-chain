INTERFACE zif_ai_types
  PUBLIC .
  "--------------------------------
  " Naming Conventions:
  " ty_  = Type
  " ts_  = Structure
  " tt_  = Table Type
  " th_  = Hashed Table Type
  " te_  = Enum Type
  "--------------------------------
  "Shared types for node_id and agent_id
  TYPES ty_node_id TYPE uuid.
  TYPES ty_agent_id TYPE uuid. "Could be UUID
  " Shared Structure for the state
  TYPES: BEGIN OF ts_graph_state,
           messages       TYPE string,  "LLM responses / narrative"
           last_tool_name TYPE string,  "debug info"
           branch_label   TYPE string,  "routing: ON_LABEL"
           result_json    TYPE string,  "final output"
         END OF ts_graph_state.
  " Shared Structure for tool registry
  TYPES: BEGIN OF ts_tool_registry,
           tool_name        TYPE string,
           tool_endpoint    TYPE REF TO zif_ai_tool,
           tool_description TYPE string,
         END OF ts_tool_registry.
  " Hashed table for tools to look up by name
  " We could create a unique tool_id in case there are duplicate names
  TYPES th_tool_registry_map
    TYPE HASHED TABLE OF ts_tool_registry
    WITH UNIQUE KEY tool_name.
  " Shared Structure for storing the graph structure as a hashed table
  " Definition of edge
  TYPES: BEGIN OF ts_edge,
           target_node_id TYPE ty_node_id,
           target_node    TYPE REF TO zif_ai_node, " direct reference to the node object
           condition      TYPE string,
           condition_value type string,
           priority type i, " e.g. 'ALWAYS', 'ON_LABEL:success' Could also be a enum
         END OF ts_edge.
  " the list of edges
  TYPES tt_edge_list TYPE STANDARD TABLE OF ts_edge WITH EMPTY KEY.
  " the Map Entry
  TYPES: BEGIN OF ts_graph_entry,
           source_node_id TYPE ty_node_id,
           source_node    TYPE REF TO zif_ai_node, " direct reference to the node object
           next_nodes     TYPE tt_edge_list,
         END OF ts_graph_entry.
  " the Graph Map
  TYPES th_graph_map
    TYPE HASHED TABLE OF ts_graph_entry
    WITH UNIQUE KEY source_node_id.



  " -------------------------------
  " Test Lukas Hager
  TYPES: BEGIN OF ty_edge_lh,
           source_node_id TYPE ty_node_id,
           target_node_id TYPE ty_node_id,
           target_node    TYPE REF TO zif_ai_node,
           condition      TYPE string,
           condition_value TYPE string,
           priority       TYPE i,
         END OF ty_edge_lh.

  TYPES tt_edge_lh TYPE STANDARD TABLE OF ty_edge_lh WITH DEFAULT KEY.

  " Node registry entry: id + node ref
  TYPES: BEGIN OF ty_node_entry_lh,
           node_id TYPE ty_node_id,
           node    TYPE REF TO zif_ai_node,
         END OF ty_node_entry_lh.

  TYPES tt_node_registry_lh TYPE STANDARD TABLE OF ty_node_entry_lh
                         WITH DEFAULT KEY.


CONSTANTS:
    gc_cond_always     TYPE string VALUE 'ALWAYS',
    gc_cond_on_control TYPE string VALUE 'ON_CONTROL'.


ENDINTERFACE.
