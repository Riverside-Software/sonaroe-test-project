using Progress.Lang.*.
using OpenEdge.Web.WebResponseWriter.
using OpenEdge.Net.HTTP.StatusCodeEnum.
using OpenEdge.Web.WebHandler.
using Progress.Json.ObjectModel.JsonObject.

block-level on error undo, throw.

class rssw.EchoHandler inherits WebHandler:

    define temp-table ttCustomer like Customer.
    define dataset dsCustomer for ttCustomer.

    method override protected integer HandleGet( input poRequest as OpenEdge.Web.IWebRequest ):
        define variable oBody     as OpenEdge.Core.String            no-undo.
        define variable oResponse as OpenEdge.Net.HTTP.IHttpResponse no-undo.
        define variable oWriter   as OpenEdge.Web.WebResponseWriter  no-undo.

        define variable httEmployee    as handle     no-undo.
        define variable lcjsonEmployee as longchar   no-undo.
        define variable ipause         as integer    no-undo.

        ipause = integer(poRequest:getPathParameter("pause")) no-error.
        find first customer where custnum = ipause no-lock.
        create ttCustomer.
        buffer-copy customer to ttCustomer.

        assign
             oResponse            = new OpenEdge.Web.WebResponse()
             .
        buffer ttCustomer:write-json('LONGCHAR', lcjsonEmployee, true).

        assign
             oResponse:Entity        = new OpenEdge.Core.String(lcjsonEmployee)
             oResponse:ContentType   = 'application/json':u
             oResponse:ContentLength = length(lcjsonEmployee)
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
