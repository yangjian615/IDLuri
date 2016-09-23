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
;-
;*****************************************************************************************
;+
;   Get the URL
;
; :Params:
;       URI:            out, required, type=string
;                       The URI to be made the current uri.
;-
function MrFileURI::Get, uri_pattern, tstart, tend, $
COUNT=count, $
CLOSEST=closest, $
NEWEST=newest, $
TIMEORDER=timeorder, $
TPATTERN=tpattern, $
VERSION=version, $
VREGEX=vregex
	compile_opt idl2
	on_error, 2
	
	;Search for the files
	files = self -> Search(uri_pattern, COUNT=count)
	if count eq 0 then return, ''

	;Filter by time
	files = self -> FilterTime( uri_pattern, files, tstart, tend, $
	                            COUNT     = count, $
	                            CLOSEST   = closest, $
	                            TIMEORDER = timeorder, $
	                            TPATTERN  = tpattern )
	if count eq 0 then message, 'No files in time interval.'
	
	;Filter by version
	files = self -> FilterVersion( files, $
	                               COUNT   = count, $
	                               NEWEST  = newest, $
	                               VERSION = version, $
	                               VREGEX  = vregex )
	if count eq 0 then message, 'No files pass version filter.'
	
	;Return
	return, files
end


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
;   Set the URL.
;
; :Params:
;       URL:            in, required, type=string
;                       The URL to be made the current url.
;-
pro MrFileURI::SetURI, uri
	compile_opt idl2
	on_error, 2

	;Parse the URL
	;   - Take scheme from object property -- relative path could be given.
	;   - Implies the scheme property must be correct upon entry
	self -> ParseURI, uri, $
	                  FRAGMENT  = fragment, $
	                  HOST      = host, $
	                  PASSWORD  = password, $
	                  PATH      = path, $
	                  PORT      = port, $
	                  QUERY     = query, $
	                  SCHEME    = scheme, $
	                  USERNAME  = username

	;Test if the path is a directory
	if scheme eq '' then message, 'Invalid URI.'
	if ~file_test(path, /DIRECTORY) then $
		message, 'URI path is not a valid directory: "' + path + '".'
	
	;Change directories
	cd, path
	
	;Set properties
	self.fragment  = fragment
	self.host      = host
	self.password  = password
	self.path      = path
	self.port      = port
	self.query     = query
	self.scheme    = scheme
	self.username  = username
end


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
; :Returns:
;       If successful, a valid MrFileURI object will be returned.
;-
function MrFileURI::init, uri
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Initialize superclasses
	success = self -> MrURI::Init(uri)
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
;       ALL:            Display all links?
;       DELAY:          Max time to wait for a double click.
;       LOCAL_LIST:     List of files in the local directory to which files are saved.
;       LOCAL_PWD:      Present working directory upon initialization.
;       NCLICKS:        Number of clicks: single or double.
;       TLB:            Widget ID of the top-level base.
;       WEB_LIST:       List of links at the current web path.
;       WEBGET:         Object for navigating and downloading from the web.
;-
pro MrFileURI__Define, class
	compile_opt idl2

	class = { MrFileURI, $
	          inherits MrURI $
	        }
end