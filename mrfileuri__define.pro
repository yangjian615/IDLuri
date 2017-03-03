; docformat = 'rst'
;
; NAME:
;       MrFileURI__Define
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
;+
;   Provide a GUI, similar to Dialog_Pickfile, for downloading files from the internet.
;
; :See Also:
;   MrParseURL.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016-06-24  -   Written by Matthew Argall.
;       2017-01-23  -   Utilize superclass more effectively. - MRA
;-
;*****************************************************************************************
;+
;   Get the directory listings.
;
; :Private:
;-
function MrFileURI::GetDirList, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
	;Get the directory contents
	dirList = file_search(COUNT=count, /MARK_DIRECTORY)
	
	return, dirList
end


;+
;   Set the URI.
;
;   Calling Sequence:
;       oURI -> SetURI, uri
;       oURI -> SetURI, KEYWORD=value, ...
;
; :Params:
;       URI:            in, required, type=string
;                       The URL to be made the current uri. If present, all keywords
;                           are ignored.
;       SUCCESS:        out, optional, type=boolean
;                       Flag indicating that the URI was set successfully (true) or
;                           unsuccessfully (false). Set to a named variable to suppress
;                           error reporting.
;
; :Keywords:
;       FRAGMENT:       in, optional, type=string
;                       URL fragment; appears at end trailing a #.
;       HOST:           in, optional, type=string
;                       Host of the URL address.
;       PATH:           in, optional, type=string
;                       Path of the URL destination.
;       PORT:           in, optional, type=string
;                       Port to use for connection.
;       QUERY:          in, optional, type=string
;                       Query string, appearing after the ?.
;       SCHEME:         in, optional, type=string
;                       URL scheme.
;-
PRO MrFileURI::SetURI, uri, success, $
FRAGMENT=fragment, $
HOST=host, $
PATH=path, $
PORT=port, $
QUERY=query, $
SCHEME=scheme
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		On_Error, 2
		success = 0B
		IF ~Arg_Present(success) THEN Message, /REISSUE_LAST
		RETURN
	ENDIF
	
	;Assume failure
	success = 0B
	
	;Parse the URI if it was given
	IF N_Elements(uri) GT 0 THEN BEGIN
		self -> ParseURI, uri, $
		                  FRAGMENT     = fragment, $
		                  HOST         = host, $
		                  PASSWORD     = password, $
		                  PATH         = path, $
		                  PORT         = port, $
		                  QUERY        = query, $
		                  SCHEME       = scheme, $
		                  USERNAME     = username
		IF scheme EQ '' THEN Message, 'URI could not be parsed: "' + uri + '".'
	ENDIF
	
	IF N_Elements(scheme) GT 0 THEN BEGIN
		IF scheme NE 'file' $
			THEN Message, 'SCHEME must be "file".' $
			ELSE self.scheme = scheme
	ENDIF
	
	IF N_Elements(path) GT 0 THEN BEGIN
		IF ~file_test(path, /DIRECTORY) $
			THEN Message, 'Invalid PATH: "' + path + '".' $
			ELSE self.path = path
	ENDIF
	
	;Set URL properties
	IF N_Elements(fragment) GT 0 THEN self.fragment = fragment
	IF N_Elements(host)     GT 0 THEN self.host     = host
	IF N_Elements(password) GT 0 THEN self.password = password
	IF N_Elements(port)     GT 0 THEN self.port     = port
	IF N_Elements(query)    GT 0 THEN self.query    = query
	IF N_Elements(username) GT 0 THEN self.username = username
	
	;Change directories
	CD, self.path
	success = 1B
END


;+
;   Cleanup after the object is destroyed.
;-
pro MrFileURI::Cleanup
	compile_opt idl2
	on_error, 2

	self -> MrURI::Cleanup
end


;+
;   The initialization method.
;
; :Params:
;       URI:            in, optional, type=string
;                       File identifier.
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrURI::Init is also accepted here.
;
; :Returns:
;       If successful, a valid MrFileURI object will be returned.
;-
function MrFileURI::init, uri, $
_REF_EXTRA=extra
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Initialize superclasses
	success = self -> MrURI::Init(uri, _STRICT_EXTRA=extra)
	if success eq 0 then return, 0

	return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:              out, optional, type=structure
;                           Class definition structure.
;
; :Fields:
;-
pro MrFileURI__Define, class
	compile_opt idl2

	class = { MrFileURI, $
	          inherits MrURI $
	        }
end