INTERFACE zif_ai_tool
  PUBLIC .
  METHODS execute
    IMPORTING
        input         TYPE string  "input data to the tool
        calling_node  TYPE REF TO zif_ai_node OPTIONAL
    EXPORTING output        TYPE string "output of the tool
    RETURNING VALUE(status) TYPE i. "0 success, non-zero error

  " Tool identity
  METHODS get_name
    RETURNING
      VALUE(name) TYPE string.

  METHODS get_description
    RETURNING
      VALUE(description) TYPE string.

  METHODS get_tool_type
    RETURNING
      VALUE(tool_type) TYPE string.

  METHODS get_argument_metadata
    RETURNING VALUE(arguments) TYPE zcl_tool_schema=>tt_tool_arguments.
ENDINTERFACE.
