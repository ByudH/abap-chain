CLASS zcl_ai_orchestrator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        it_nodes         TYPE zif_ai_types=>tt_node_registry_lh
        it_edges         TYPE zif_ai_types=>tt_edge_lh
        iv_start_node_id TYPE zif_ai_types=>ty_node_id
        is_initial_state TYPE zif_ai_types=>ty_graph_state OPTIONAL
      RETURNING
        VALUE(rs_final_state) TYPE zif_ai_types=>ty_graph_state.

ENDCLASS.


CLASS zcl_ai_orchestrator IMPLEMENTATION.

  METHOD run.
    DATA ls_state TYPE zif_ai_types=>ty_graph_state.
    ls_state = is_initial_state.

    DATA lv_current_node_id TYPE zif_ai_types=>ty_node_id.
    lv_current_node_id = iv_start_node_id.

    IF lv_current_node_id IS INITIAL.
      " No start node -> nothing to do
      rs_final_state = ls_state.
      RETURN.
    ENDIF.

    DATA ls_node_entry      TYPE zif_ai_types=>ty_node_entry_lh.
    DATA ls_edge            TYPE zif_ai_types=>ty_edge_lh.
    DATA lt_edges_for_node  TYPE zif_ai_types=>tt_edge_lh.
    DATA lv_next_node_id    TYPE zif_ai_types=>ty_node_id.

    " Optional: avoid infinite loops in bad graphs
    DATA lv_step_count TYPE i VALUE 0.
    CONSTANTS lc_max_steps TYPE i VALUE 50.

    WHILE lv_current_node_id IS NOT INITIAL.

      lv_step_count = lv_step_count + 1.
      IF lv_step_count > lc_max_steps.
        " Safety break: prevent endless loops in broken graphs
        EXIT.
      ENDIF.


      " 1) Find current node in registry
      CLEAR ls_node_entry.
      READ TABLE it_nodes INTO ls_node_entry
           WITH KEY node_id = lv_current_node_id.

      IF sy-subrc <> 0 OR ls_node_entry-node IS INITIAL.
        " Inconsistent graph: node id not found → stop
        EXIT.
      ENDIF.


      " 2) Execute node (polymorphic via zif_ai_node)
      ls_state = ls_node_entry-node->execute(
                   state_input = ls_state ).


      " 3) Collect outgoing edges for this node
      CLEAR lt_edges_for_node.
      LOOP AT it_edges INTO ls_edge
           WHERE source_node_id = lv_current_node_id.
        APPEND ls_edge TO lt_edges_for_node.
      ENDLOOP.

      IF lt_edges_for_node IS INITIAL.
        " No outgoing edges → implicit END
        EXIT.
      ENDIF.

      " Sort edges by priority (lower = higher priority)
      SORT lt_edges_for_node BY priority ASCENDING.

      " 4) Determine next node:
      "    a) ON_CONTROL: condition_value = state.branch_label
      "    b) ALWAYS: unconditional fallback
      CLEAR lv_next_node_id.

      " First: ON_CONTROL
      LOOP AT lt_edges_for_node INTO ls_edge
           WHERE condition       = zif_ai_types=>gc_cond_on_control
             AND condition_value = ls_state-branch_label.
        lv_next_node_id = ls_edge-target_node_id.
        EXIT.
      ENDLOOP.

      " If no ON_CONTROL matched, try ALWAYS
      IF lv_next_node_id IS INITIAL.
        LOOP AT lt_edges_for_node INTO ls_edge
             WHERE condition = zif_ai_types=>gc_cond_always.
          lv_next_node_id = ls_edge-target_node_id.
          EXIT.
        ENDLOOP.
      ENDIF.

      " No matching edge -> terminate
      IF lv_next_node_id IS INITIAL.
        EXIT.
      ENDIF.

      " Move to next node
      lv_current_node_id = lv_next_node_id.

    ENDWHILE.

    rs_final_state = ls_state.

  ENDMETHOD.

ENDCLASS.
