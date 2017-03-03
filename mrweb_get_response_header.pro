; docformat = 'rst'
;
; NAME:
;       MrWeb_Get_Response_Header
;
;*****************************************************************************************
;   Copyright (c) 2017, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
; PURPOSE:
;+
;   Get the response header via ::Get without downloading the URL content.
;
;   Calling Sequence
;       rh = MrWeb_Get_Response_Header(url)
;       rh = MrWeb_Get_Response_Header(NETURL=neturl)
;
; :Categories:
;       MrWeb
;
; :Params:
;       URL:                in, optional, type=string
;                           URL for which the response header is desired. If not
;                               provided, `NETURL` must be given.
;
; :Keywords:
;       NETURL:             in, optional, type=IDLnetURL objref
;                           The IDLnetURL object used to obtain the response header.
;                               If not provided, or the object is invalid, a new IDLnetURL
;                               object is created.
;
; :Returns:
;       RESPONSE_HEADER:    The response header of the `URL`.
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
;       2017/02/02  -   Written by Matthew Argall
;-
FUNCTION MrWeb_Get_Response_Header, uri, $
NETURL=neturl
	compile_opt idl2
	On_Error, 2
	
	;Check inputs
	IF N_Elements(uri) EQ 0 THEN BEGIN
		IF ~Obj_Valid(neturl) THEN Message, 'If URL is undefined, NETURL must be given.'
	ENDIF
	IF ~Obj_Valid(neturl) THEN neturl = Obj_New('IDLnetURL')

	;Change the callback function
	neturl -> GetProperty, CALLBACK_FUNCTION=callback_in
	neturl -> SetProperty, CALLBACK_FUNCTION='MrWeb_Callback_Response_Header'

	;Get the URL, but short-circuit as soon as the header is downloaded
	;   - Use a catch because cancelling the callback FUNCTION will trigger an error
	;   - Do not set a default URL so that the username and password will be sent
	Catch, the_error
	IF the_error EQ 0 $
		THEN output = neturl -> Get(URL=uri, /BUFFER) $
		ELSE Catch, /CANCEL

	;Get the response header
	neturl -> GetProperty, RESPONSE_HEADER=response_header

	;Reset the callback FUNCTION
	neturl -> SetProperty, CALLBACK_FUNCTION=callback_in
	
	IF ~Arg_Present(neturl) THEN Obj_Destroy, neturl
	RETURN, response_header
END
