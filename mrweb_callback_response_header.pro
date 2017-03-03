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
;   Callback function for IDLnetURL::Get that cancels download after the repsonse
;   header is received.
;
; :Categories:
;       MrWeb
;
; :Params:
;       STATUSINFO:         in, required, type=strarr
;                           Status information about the Get or Put methods.
;       PROGRESSINFO:       in, required, type=lon64arr
;                           Contains progress information. Elements::
;                               0 - 1=valid data, 0=invalid data
;                               1 - Download total for Get
;                               2 - Number of bytes downloaded
;                               3 - Upload total for Put
;                               4 - Number of bytes uploaded
;       CALLBACKDATA:       in, optional, type=depends
;                           User data passed into the callback function.
;
; :Returns:
;       CONTINUE:           1 if operation should continue, 0 if operation should
;                               be halted.
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
function MrWeb_Callback_Response_Header, statusInfo, progressInfo, callbackData
	compile_opt idl2

	;The response header does not contribute to the download total.
	if (progressInfo[0] eq 1) && (progressInfo[2] gt 0) $
		then return, 0 $
		else return, 1
end
