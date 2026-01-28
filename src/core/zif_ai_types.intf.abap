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

  " Message role constants
  CONSTANTS:
    gc_role_system    TYPE string VALUE 'system',
    gc_role_user      TYPE string VALUE 'user',
    gc_role_assistant TYPE string VALUE 'assistant',
    gc_role_error     TYPE string VALUE 'error', " for the error messages in state
    gc_role_tool      TYPE string VALUE 'tool'.

  TYPES: BEGIN OF ts_message,
           role    TYPE string,  "e.g., 'user', 'assistant', 'system', 'tool'
           content TYPE string,  "The actual message content
         END OF ts_message.

  TYPES tt_messages TYPE STANDARD TABLE OF ts_message WITH EMPTY KEY.


  " Shared Structure for the state
  TYPES: BEGIN OF ts_graph_state,
           messages              TYPE tt_messages,  "LLM responses / narrative"
           last_tool_name        TYPE string,  "debug info"
           branch_label          TYPE string,  "routing: ON_LABEL"
           result_json           TYPE string,  "final output"
           tool_arguments        TYPE string,
           status                TYPE string,  "workflow execution status (e.g., RETRYING, ERROR)"

           " HITL
           hitl_correlation_id   TYPE sysuuid_x16,
           hitl_topic            TYPE string,
           hitl_reason           TYPE string,
           hitl_prompt           TYPE string,
           hitl_response_schema  TYPE string,  " JSON schema string
           hitl_primary_field    TYPE string,  " e.g. 'approved'
           hitl_response_payload TYPE string,  " response JSON string
           skip_current_execute  TYPE ABAP_Boolean, "skip current node after hitl resume"
           last_checkpoint_id    TYPE zai_checkpoint-checkpoint_id,
           paused_checkpoint_id  TYPE zai_checkpoint-checkpoint_id,
           hitl_strategy         TYPE string, " WAIT for blocking and PAUSE for non-blocking

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
           target_node_id  TYPE ty_node_id,
           target_node     TYPE REF TO zif_ai_node, " direct reference to the node object
           condition       TYPE string,
           condition_value TYPE string,
           priority        TYPE i, " e.g. 'ALWAYS', 'ON_LABEL:success' Could also be a enum
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

  " --- PERSISTENCE TYPES (For JSON Storage) ---

  " 1. Edge Blueprint (No Object References)
  TYPES: BEGIN OF ts_edge_blueprint,
           target_node_id  TYPE ty_node_id,   " We only need the ID to link
           condition       TYPE string,
           condition_value TYPE string,
           priority        TYPE i,
         END OF ts_edge_blueprint.

  TYPES tt_edge_blueprints TYPE STANDARD TABLE OF ts_edge_blueprint WITH EMPTY KEY.

  " 2. Node Blueprint (Replaces Object with Class Name)
  TYPES: BEGIN OF ts_node_blueprint,
           node_id    TYPE ty_node_id,
           node_name  TYPE string,
           class_name TYPE string,           " <--- THE FIX: Store 'ZCL_NODE_LLM', etc.
           config     TYPE string,           " Optional: Node specific settings
           next_nodes TYPE tt_edge_blueprints,
         END OF ts_node_blueprint.

  " 3. The Full Map for Storage
  TYPES tt_graph_blueprint TYPE STANDARD TABLE OF ts_node_blueprint WITH EMPTY KEY.

  " 1. TOOL BLUEPRINT (For Storage/DB/JSON)
  TYPES: BEGIN OF ts_tool_blueprint,
           tool_name        TYPE string,
           tool_class       TYPE string,  " The ABAP Class name (e.g. 'ZCL_AI_TOOL_SEARCH')
           tool_description TYPE string,
           configuration    TYPE string," in case name and description differs from the orchestrator
         END OF ts_tool_blueprint.

  TYPES: tt_tool_blueprints TYPE STANDARD TABLE OF ts_tool_blueprint WITH EMPTY KEY.

  " struture for agent blueprint enabling a single return sturture and method nesting
  TYPES: BEGIN OF ts_agent_blueprint,
           agent_id                TYPE ty_agent_id,
           agent_name              TYPE string,
           start_node_id           TYPE ty_node_id,
           graph_blueprint         TYPE tt_graph_blueprint,
           tool_registry_blueprint TYPE tt_tool_blueprints,
         END OF ts_agent_blueprint.



  " =========================================================
  "  NEW TYPES AND CONSTANTS FOR ERROR HANDLING (REQUIRED BY ZCX_AI_AGENT_ERROR)
  " =========================================================

  " Type for severity level (used in ZCX_AI_AGENT_ERROR)
  TYPES ty_severity TYPE i.

  " Severity Constants (used in ZCX_AI_AGENT_ERROR implementation)
  CONSTANTS:
    gc_severity_info  TYPE ty_severity VALUE 1,
    gc_severity_warn  TYPE ty_severity VALUE 2,
    gc_severity_error TYPE ty_severity VALUE 3, " Used for API errors
    gc_severity_fatal TYPE ty_severity VALUE 4.

  " Workflow Status Constants (used in ZCL_AI_NODE_LLM for state update)
  CONSTANTS:
    gc_workflow_status_new      TYPE string VALUE 'NEW',
    gc_workflow_status_running  TYPE string VALUE 'RUNNING',
    gc_workflow_status_retrying TYPE string VALUE 'RETRYING',  " Status set on Timeout
    gc_workflow_status_finished TYPE string VALUE 'FINISHED',
    gc_workflow_status_error    TYPE string VALUE 'ERROR'.     " Status set on API failure

  " HITL
  CONSTANTS gc_workflow_status_waiting  TYPE string VALUE 'WAITING_FOR_HUMAN'.
  CONSTANTS gc_workflow_status_paused  TYPE string VALUE 'PAUSED_FOR_HITL'.

  CONSTANTS gc_hitl_strategy_wait TYPE string VALUE 'WAIT'.
  CONSTANTS gc_hitl_strategy_pause TYPE string VALUE 'PAUSE'.
  " =========================================================


  CONSTANTS:
    gc_cond_always     TYPE string VALUE 'ALWAYS',
    gc_cond_on_control TYPE string VALUE 'ON_CONTROL'.


ENDINTERFACE.
