INTERFACE zif_ai_node_tool_aware
PUBLIC.

  " Register a tool for this node
  METHODS add_tool
    IMPORTING
      tool_name TYPE string
      tool      TYPE REF TO zif_ai_tool
      description TYPE string OPTIONAL.

  " Get all tools registered on this node (for LLM -> tool selection, orchestration, etc.)
  METHODS get_tools
    RETURNING VALUE(tools) TYPE zif_ai_types=>th_tool_registry_map.

  " Optional: convenience method to get a single tool by name
  METHODS get_tool
    IMPORTING
      tool_name TYPE string
    RETURNING
      VALUE(tool) TYPE REF TO zif_ai_tool.

ENDINTERFACE.
