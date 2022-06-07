; read_cdf.pro is a generic routine for reading '.cdf' files
; returns all variables into a structure
; example call: a = read_cdf('test.cdf')

;NOTES:
; There are two different kinds of data stored in cdf files: rvariables and zvariables.  rvariables, or "Regular" variables, have to conform to a rigid structures (all rvariables in a file have to have the same dimension, which is defined ahead of time). zvariables are more flexible (no importance in the letter "z", just a name).

;;;
;Reading CDF Files
;The following built-in IDL commands are used to read data from a CDF file:
;CDF_OPEN: Open an existing CDF file.
;CDF_INQUIRE: Call this function to find the general information about the contents of the CDF file.
;CDF_CONTROL: Call this function to obtain further information about the CDF file
;CDF_VARINQ: Retrieve the names, types, sizes, and other information about the variables in the CDF file.
;CDF_VARGET: Retrieve the variable values.
;CDF_ATTINQ: Optionally, retrieve the names, scope and other information about the CDFs attributes.
;CDF_ATTGET: Optionally, retrieve the attributes.
;CDF_CLOSE: Close the file.
;If the structure of the CDF file is already known, the inquiry routines do not need to be called--only CDF_OPEN, CDF_ATTGET, CDF_VARGET, and CDF_CLOSE would be needed.
;;;


FUNCTION read_cdf, file, SILENT = SILENT

   cdf_var = cdf_open(file)

   id = CDF_OPEN(file)
   str = cdf_inquire(id)
      ;varstr = cdf_varinq(id, 0)

   nnvars = str.nvars   ; number of r-variables
   nnzvars = str.nzvars ; number of z-variables

   ndim = lonarr(nnvars + nnzvars)
   rec_counts = lonarr(nnvars + nnzvars) ; "record counts" (i.e. number of measurements in the file)
   names = strarr(nnvars + nnzvars)

   IF nnvars GT 0 THEN BEGIN
      FOR i = 0,nnvars -1L do begin
         varstr = cdf_varinq(id, i)
         names[i] = varstr.name
         ndim[i] = varstr.dimvar[0]      ; sometimes dim might have >1 elements? Not sure how to handle this case...
         IF not(KEYWORD_SET(silent)) THEN help, varstr
         cdf_control, id, get_var_info = yo, variable = names[i]
         rec_counts[i] = yo.maxrec 
      ENDFOR
   ENDIF

   IF nnzvars GT 0 THEN BEGIN
      FOR i = 0,nnzvars -1L do begin
         varstr = cdf_varinq(id, i, /zvariable)
         names[nnvars + i] = varstr.name
         ndim[nnvars + i] = varstr.dim[0]      ; sometimes dim might have >1 elements? Not sure how to handle this case...
         IF not(KEYWORD_SET(silent)) THEN help, varstr
         cdf_control, id, get_var_info = yo, variable = names[i]
         rec_counts[nnvars + i] = yo.maxrec 
      ENDFOR
   ENDIF

   IF NOT(KEYWORD_SET(silent)) THEN print, names

   for i=0L, n_elements(rec_counts)-1L do begin
      if rec_counts[i] EQ 0 THEN rec_counts[i] = 1   ; prevents some kind of error with CDF_VARGET
   endfor

   FOR i = 0L, n_elements(names) - 1L DO BEGIN
      ON_IOERROR, nodata
      bad = 1
      IF names[i] NE '' THEN void = EXECUTE("CDF_VARGET, id, '" + names[i] + "', " + names[i] + ", rec_count = rec_counts[i]")
      bad = 0
      nodata: IF bad THEN names[i] = ''
   ENDFOR

   ind = n_elements(names)-1L
   while names[ind] eq '' do ind--
   names = names[0:ind]

   return_string = 'output = {'

   FOR i = 0L, n_elements(names) - 1L DO BEGIN
      IF names[i] NE '' THEN BEGIN
         return_string = return_string + names[i] + ':' + names[i]
         IF i NE n_elements(names)-1L AND names[i] NE '' THEN return_string = return_string + ','
      ENDIF
   ENDFOR
   return_string = return_string + '}'
   ; return_string = return_string + '}, names'

   void = EXECUTE(return_string)  ; returns structure

   CDF_CLOSE, id

   scratch_files = file_search('TMP*.cdf')    ; erase temporary scratch files that were created by idl cdf routines
   if not(n_elements(scratch_files) eq 1 and scratch_files[0] eq '') then for i = 0L, n_elements(scratch_files)-1L do file_delete, scratch_files[i]
   scratch_files = file_search('TMP*.stg')    ; erase temporary scratch files that were created by idl cdf routines
   if not(n_elements(scratch_files) eq 1 and scratch_files[0] eq '') then for i = 0L, n_elements(scratch_files)-1L do file_delete, scratch_files[i]

   RETURN, output

END

