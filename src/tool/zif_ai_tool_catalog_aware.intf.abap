INTERFACE zif_ai_tool_catalog_aware PUBLIC.
  METHODS set_tool_catalog
    IMPORTING tool_catalog TYPE zif_ai_types=>th_tool_registry_map.
ENDINTERFACE.
