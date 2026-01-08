CLASS zcl_ai_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS generate_uuid
      RETURNING VALUE(generated_uuid) TYPE uuid.

    CLASS-METHODS state_to_json
      IMPORTING
        state         TYPE zif_ai_types=>ts_graph_state
      RETURNING
        VALUE(json)   TYPE string.

    " Deserializes JSON String back to State Structure
    CLASS-METHODS json_to_state
      IMPORTING
        json          TYPE string
      RETURNING
        VALUE(state)  TYPE zif_ai_types=>ts_graph_state.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_utils IMPLEMENTATION.
  METHOD generate_uuid.
    TRY.
        generated_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error into data(error).
        " If UUID generation fails, raise a short dump with the error message
        RAISE SHORTDUMP error.
    ENDTRY.
  ENDMETHOD.

  METHOD state_to_json.
    " 1. Create XCO Data Representation
    "    Default behavior: Keeps ABAP field names (snake_case)
    DATA(xco_data) = xco_cp_json=>data->from_abap( state ).

    " 2. Convert to String
    json = xco_data->to_string( ).
  ENDMETHOD.

  METHOD json_to_state.
    " 1. Parse JSON String
    DATA(xco_data) = xco_cp_json=>data->from_string( json ).

    " 2. Write directly to ABAP Structure
    "    Since names match exactly, no mapping logic is needed
    xco_data->write_to( REF #( state ) ).
  ENDMETHOD.

ENDCLASS.
