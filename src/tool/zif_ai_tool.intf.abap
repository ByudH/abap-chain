INTERFACE zif_ai_tool
  PUBLIC .
METHODS execute
    IMPORTING iv_input TYPE string  "input data to the tool
    EXPORTING ev_output TYPE string "output of the tool
    RETURNING VALUE(ev_status) TYPE i. "0 success, non-zero error
ENDINTERFACE.
