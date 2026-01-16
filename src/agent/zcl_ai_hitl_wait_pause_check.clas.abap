CLASS zcl_ai_hitl_wait_pause_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_hitl_wait_strategy.

ENDCLASS.



CLASS zcl_ai_hitl_wait_pause_check IMPLEMENTATION.


  METHOD zif_ai_hitl_wait_strategy~handle_wait.

    IF state-status <> zif_ai_types=>gc_workflow_status_waiting
       OR state-hitl_correlation_id IS INITIAL.
      RETURN.
    ENDIF.

    state-status = zif_ai_types=>gc_workflow_status_paused.
    state-branch_label = 'PAUSED'.

    IF state-last_checkpoint_id IS NOT INITIAL.
      state-paused_checkpoint_id = state-last_checkpoint_id.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
