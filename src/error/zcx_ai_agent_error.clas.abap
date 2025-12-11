CLASS zcx_ai_agent_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check. " Inherit from CX_STATIC_CHECK for mandatory handling

  PUBLIC SECTION.
    INTERFACES if_t100_message.  " Interface for standard message handling
    INTERFACES if_t100_dyn_msg.

    " Static constants used by the LLM Node to distinguish error types (e.g., Timeout vs. API Error)
    CONSTANTS:
      timeout   TYPE string VALUE 'TIMEOUT',   " Identifier for LLM API timeout errors (maps to RETRYING status)
      api_error TYPE string VALUE 'API_ERROR'. " Identifier for general LLM API application/service failures (maps to ERROR status)

    " Attributes to store additional context information
    DATA severity TYPE zif_ai_types=>ty_severity READ-ONLY. " Custom severity level (from ZIF_AI_TYPES)
    DATA message  TYPE string READ-ONLY.                     " Detailed, custom error message for context
    DATA error_id TYPE string READ-ONLY.                     " Stores the distinguishing error constant (e.g., 'TIMEOUT')

    " Constructor: Simplified structure. PREVIOUS and TEXTID are removed from IMPORTING
    " to bypass known type conflicts (like SOTR_CONC vs. REF TO CX_ROOT)
    METHODS constructor
      IMPORTING
        !error_id   TYPE string OPTIONAL                  " Passes the specific error identifier (e.g., 'TIMEOUT')
        " !previous LIKE cx_root OPTIONAL <--- Removed to avoid SOTR_CONC conflict
        !severity   TYPE zif_ai_types=>ty_severity OPTIONAL
        !message    TYPE string OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_ai_agent_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    " 1. Call the superclass constructor with no parameters to ensure activation.
    " This bypasses the strict type check on TEXTID/PREVIOUS.
    CALL METHOD super->constructor( ).

    " 2. Initialize custom attributes
    me->severity = severity.
    me->message  = message.
    me->error_id = error_id.
  ENDMETHOD.
ENDCLASS.
