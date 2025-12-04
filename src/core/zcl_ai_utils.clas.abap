CLASS zcl_ai_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS generate_uuid
      RETURNING VALUE(generated_uuid) TYPE uuid.
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
ENDCLASS.
