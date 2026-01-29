CLASS zcx_schema_validation DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    DATA error_message TYPE string READ-ONLY.
    DATA field_name TYPE string READ-ONLY.
    DATA validation_type TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        error_message   TYPE string OPTIONAL
        field_name      TYPE string OPTIONAL
        validation_type TYPE string OPTIONAL
        previous        TYPE REF TO cx_root OPTIONAL.

ENDCLASS.

CLASS zcx_schema_validation IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->error_message = error_message.
    me->field_name = field_name.
    me->validation_type = validation_type.

    " Set default T100 key
    if_t100_message~t100key = if_t100_message=>default_textid.
  ENDMETHOD.
ENDCLASS.


