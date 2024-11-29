using Progress.Lang.*.
using OpenEdge.Web.WebResponseWriter.
using OpenEdge.Net.HTTP.StatusCodeEnum.
using OpenEdge.Web.WebHandler.
using Progress.Json.ObjectModel.JsonObject.

block-level on error undo, throw.

class rssw.CustomerHandler inherits WebHandler:

    define temp-table ttCustomer like Customer.
    define dataset dsCustomer for ttCustomer.

    method override protected integer HandleGet( input poRequest as OpenEdge.Web.IWebRequest ):

        define variable oResponse as OpenEdge.Net.HTTP.IHttpResponse no-undo.
        define variable oWriter   as OpenEdge.Web.WebResponseWriter  no-undo.

        define variable lcJson as longchar   no-undo.
        define variable iNum   as integer    no-undo.

        iNum = integer(poRequest:getPathParameter("num")) no-error.
        find first customer where custnum = iNum no-lock.
        create ttCustomer.
        buffer-copy customer to ttCustomer.

        assign
             oResponse            = new OpenEdge.Web.WebResponse()
             .
        buffer ttCustomer:write-json('LONGCHAR', lcJson, true).

        assign
             oResponse:Entity        = new OpenEdge.Core.String(lcJson)
             oResponse:ContentType   = 'application/json':u
             oResponse:ContentLength = length(lcJson)
             oResponse:StatusCode    = integer(StatusCodeEnum:OK)
             .

         assign
             oWriter = new WebResponseWriter(oResponse).

         oWriter:Open().
         oWriter:Close().

        return 0.

     end method.

    method override protected integer HandleNotAllowedMethod( input poRequest as OpenEdge.Web.IWebRequest ):

        undo, throw new Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

    end method.

    method override protected integer HandleNotImplemented( input poRequest as OpenEdge.Web.IWebRequest ):

        undo, throw new Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

    end method.

end class.
