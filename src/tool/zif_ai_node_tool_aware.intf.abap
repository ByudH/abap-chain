INTERFACE zif_ai_node_tool_aware
PUBLIC.

  " Register a tool for this node
  METHODS add_tool
    IMPORTING
      iv_tool_name TYPE string
      io_tool      TYPE REF TO zif_ai_tool
      iv_description TYPE string OPTIONAL.

  " Get all tools registered on this node (for LLM -> tool selection, orchestration, etc.)
  METHODS get_tools
    RETURNING VALUE(rt_tools) TYPE zif_ai_types=>tool_registry_map.

  " Optional: convenience method to get a single tool by name
  METHODS get_tool
    IMPORTING
      iv_tool_name TYPE string
    RETURNING
      VALUE(ro_tool) TYPE REF TO zif_ai_tool.

ENDINTERFACE.
