@EndUserText.label: 'HITL Response Payload'

define abstract entity ZA_AI_HITL_RESPONSE
{
  CorrelationId   : uuid;
  ResponsePayload : abap.string;
}
