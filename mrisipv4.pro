; docformat = 'rst'
;
; NAME:
;       MrIsIPv4
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
; PURPOSE:
;+
;   Determine if a given URL host name is an IPv4 address.
;
;   About:
;       A host identified by an IPv4 literal address is represented in
;       dotted-decimal notation (a sequence of four decimal numbers in the
;       range 0 to 255, separated by ".").
;
;          IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
;
;          dec-octet   = DIGIT                 ; 0-9
;                      / %x31-39 DIGIT         ; 10-99
;                      / "1" 2DIGIT            ; 100-199
;                      / "2" %x30-34 DIGIT     ; 200-249
;                      / "25" %x30-35          ; 250-255
;
;   References:
;       Section 3.2.2 of http://www.ietf.org/rfc/rfc3986.txt
;
; :Params:
;       HOST:           in, required, type=string/strarr
;                       An IPv4 address for a host.
;
; :Keywords:
;       REGEX:          out, optional, type=string
;                       The regular expression used to check for validity.
;
; :Returns:
;       ISIPV4:         1 if `HOST` is an IPv4 addres, 0 otherwise.
;-
function MrIsIPv4, host, $
REGEX=regex
	compile_opt idl2
	on_error, 2
	
	;Regular expression
	;   Alternative forms do not allow for the /SUBEXP option to work fully.
	;         0-9   10-99      100-199   200-249     250-255
	octet = '([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])'
	regex = octet + '\.' + octet + '\.' + octet + '\.' + octet
	
	;Check if a valid IPv4 address was given
	isIPv4 = stregex(host, regex, /BOOLEAN)

	return, isIPv4
end