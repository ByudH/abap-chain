CLASS zcl_ai_planner_resp_parser DEFINITION
  PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS clean_json_string
      IMPORTING raw TYPE string
      RETURNING VALUE(clean) TYPE string.
ENDCLASS.

CLASS zcl_ai_planner_resp_parser IMPLEMENTATION.
  METHOD clean_json_string.
    clean = raw.
    clean = replace( val = clean regex = '```json' with = '' ).
    clean = replace( val = clean regex = '```' with = '' ).
    clean = condense( clean ).
  ENDMETHOD.
ENDCLASS.
