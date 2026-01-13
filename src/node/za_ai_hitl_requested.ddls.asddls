@EndUserText.label: 'HITL Requested Event Payload'

define abstract entity ZA_AI_HITL_REQUESTED
{
  ev_Topic              : abap.char(30);
  ev_CorrelationId      : uuid;
  ev_AgentId            : uuid;
  ev_NodeId             : uuid;

  ev_Reason             : abap.string;
  ev_Prompt             : abap.string;

  ev_ResponseSchema     : abap.string;
  ev_PrimaryResultField : abap.char(60);
}
