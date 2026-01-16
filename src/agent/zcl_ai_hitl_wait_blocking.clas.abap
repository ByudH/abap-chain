CLASS zcl_ai_hitl_wait_blocking DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_hitl_wait_strategy.

    METHODS constructor
      IMPORTING
        poll_seconds    TYPE i DEFAULT 2
        timeout_seconds TYPE i DEFAULT 300.

  PRIVATE SECTION.
    DATA poll    TYPE i.
    DATA timeout TYPE i.

    METHODS extract_branch_label
      IMPORTING
        payload TYPE string
        primary TYPE string
      RETURNING
        VALUE(label) TYPE string.
ENDCLASS.

CLASS zcl_ai_hitl_wait_blocking IMPLEMENTATION.

  METHOD constructor.
    poll = poll_seconds.
    timeout = timeout_seconds.
  ENDMETHOD.

  METHOD zif_ai_hitl_wait_strategy~handle_wait.

    IF state-status <> zif_ai_types=>gc_workflow_status_waiting
       OR state-hitl_correlation_id IS INITIAL.
      RETURN.
    ENDIF.

    DATA elapsed TYPE i VALUE 0.

    WHILE elapsed < timeout.

      SELECT SINGLE status, response_payload, primary_result_field
        FROM zai_hitl_req
        WHERE correlation_id = @state-hitl_correlation_id
        INTO (@DATA(db_status), @DATA(db_payload), @DATA(db_primary)).

      IF sy-subrc = 0 AND db_status = 'RESPONDED'.

        state-hitl_response_payload = db_payload.
        state-status = zif_ai_types=>gc_workflow_status_running.

        IF db_primary IS INITIAL.
          db_primary = state-hitl_primary_field.
        ENDIF.

        state-branch_label = extract_branch_label(
          payload = db_payload
          primary = CONV string( db_primary ) ).

        RETURN.
      ENDIF.

      WAIT UP TO poll SECONDS.
      elapsed = elapsed + poll.

    ENDWHILE.

    state-status = zif_ai_types=>gc_workflow_status_error.
    state-branch_label = 'TIMEOUT'.

  ENDMETHOD.

  METHOD extract_branch_label.
    " TODO: replace with real JSON parsing
    DATA primary_value TYPE string.
    primary_value = primary.

    TRANSLATE primary_value TO LOWER CASE.

    IF primary_value = 'approved'.
      IF payload CS '"approved":true'
         OR payload CS '"approved" : true'
         OR payload CS '"approved": true'.
        label = 'APPROVED'.
      ELSE.
        label = 'REJECTED'.
      ENDIF.
    ELSE.
      label = 'HITL_DONE'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
