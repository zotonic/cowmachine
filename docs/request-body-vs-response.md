Request content-type, vs accepted response
==========================================

Problem with the content types is that we end up with a multi-dimensional
matrix of input, output, and processing method functions.

The solution in Webmachine to provide a content processing function
for the ContentTypesProvided *or* ContentTypesAccepted is not enough.

In practice this means that in the process_post functions we need
to check on the content type that should be produced. This needs to
be done manually, as the ContentTypesProvided is only used for GET
requests.

Proposal is to change this, so that we always check the accepted, the
provided and the methods.  These are then passed to a generic process
function, which can then pattern match on the provided types.


1. Content-Type
    - Match against list of `content_types_accepted`
    - Only done for requests with a body.
    - Set to 'undefined' if no body present

2. Accept
    - Match against list of `content_types_provided`
    - Only done if there is an Accept header, otherwise the
      first content-type from the provided content-types
      is selected

3. Processing function

    process( Method, ContentTypeAccepted, ContentTypeProvided, Context ) ->
        % Processing
        {true | resp_body() | halt(), Context}

