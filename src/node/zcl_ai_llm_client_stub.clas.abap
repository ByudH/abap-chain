CLASS zcl_ai_llm_client_stub DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC . " Changed CREATE to PUBLIC as per your original code

  PUBLIC SECTION.
    " Static method to simulate calling an external LLM API
    CLASS-METHODS generate_completion
      IMPORTING
        !prompt           TYPE string
      RETURNING
        VALUE(completion) TYPE string
      RAISING
        zcx_ai_agent_error. " <--- MUST declare the custom exception

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_ai_llm_client_stub IMPLEMENTATION.

  METHOD generate_completion.
    " This method simulates the call to an external Large Language Model (LLM) API.
    " It includes logic to simulate specific failure conditions for testing resilience.

    " 1. Simulate Timeout Error (should be caught and lead to a RETRYING status)
    " Check if the input prompt contains a specific keyword to trigger the error.
    IF prompt CS 'SIMULATE_TIMEOUT'.
      RAISE EXCEPTION NEW zcx_ai_agent_error(
        " CORRECTION: Use ZCX_AI_AGENT_ERROR=>TIMEOUT to reference the constant
        error_id = zcx_ai_agent_error=>timeout
        severity = zif_ai_types=>gc_severity_error
        message  = 'LLM API call timed out after 30 seconds. This triggers a RETRY.'
      ).
    ENDIF.

    " 2. Simulate General API Error (should be caught and lead to an ERROR status/termination)
    " Check if the input prompt contains a specific keyword to trigger the error.
    IF prompt CS 'SIMULATE_API_ERROR'.
      RAISE EXCEPTION NEW zcx_ai_agent_error(
        " CORRECTION: Use ZCX_AI_AGENT_ERROR=>API_ERROR to reference the constant
        error_id = zcx_ai_agent_error=>api_error
        severity = zif_ai_types=>gc_severity_fatal
        message  = 'External AI Service returned HTTP 500 internal server error.'
      ).
    ENDIF.

    " 3. Simulate Successful Call
    " If no error is triggered, return a simulated LLM response.
    completion = |LLM Response Success: Processed prompt '{ prompt(50) }...' |.

  ENDMETHOD.
ENDCLASS.
