INTERFACE zif_ai_tool
  PUBLIC .
METHODS execute
    IMPORTING input TYPE string  "input data to the tool
    EXPORTING output TYPE string "output of the tool
    RETURNING VALUE(status) TYPE i. "0 success, non-zero error
ENDINTERFACE.
