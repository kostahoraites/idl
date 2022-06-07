;+
; NAME:
;
;       NEAR_NEIGHBOUR.PRO
;
; PURPOSE:
;
;       NEAR_NEIGHBOUR is used to find elements of 
;       an array that most closely correspond to the eleements
;       another array.  Especially useful for matching arrays
;       containing times. 
;
; KEYWORDS:
;
;       NO_SORT: If this keyword is set, the inputs f and array
;       are already sorted from lowest to highest value, so
;       presorting the data is not required.
;
; INPUT:
;
;       f: single variable or array of values to be matched to
;
;       array: array to be matched to data with
;       near_neighbour
;
; OUTPUT:
;
;       an array or single value with dimension = n_elements(f),
;       containing indices that point to the nearest_neighbour in variable 'array'
;
;       If either the number being compared to or its nearest neighbour is NAN, then
;       the corresponding output index is set to -1.  WARNING: when referencing a single
;       index of an array with -1, an error occurs, BUT if an array is referenced with an
;       array of indices each occurence of -1 will return the first element of the array.
;       ---> so check for -1s in the output before plotting
;
; EXAMPLE:
;
;       matching_index = near_neighbour(f, array)
;       plot, f, array[matching_index], psym = 3
;
; FILENAME:
;
;       NEAR_NEIGHBOUR.PRO
;
; MOD HISTORY:
;
;       Nov 3 2011: Stuart Bale's near_neighbour.pro adapted by 
;       Kosta Horaites to handle input arrays as well as scalars.
;
;       Aug 16 2016: added NO_SORT keyword, to speed up program
;       when inputs f and array are already sorted
;
;-

FUNCTION near_neighbour, f, array, NO_SORT = no_sort

   n = n_elements(f)

   IF n EQ 1 THEN BEGIN
   
      IF (finite(f) EQ 0) THEN RETURN, -1

      diff = abs(f - array)

      ind = WHERE(diff EQ min(diff))

      RETURN, ind[0]
   
   ENDIF ELSE BEGIN

      output = lonarr(n) - 1L              ; -1 default for matching to non-finite numbers

      ind = 0L

      n = n_elements(f)

      n_array_1 = n_elements(array) - 1L

      IF KEYWORD_SET(no_sort) THEN BEGIN

         FOR i = 0L, n - 1L DO BEGIN

            check = 1

            WHILE check AND (ind NE n_array_1) DO BEGIN

               IF abs(array[ind + 1] - f[i]) LE $
                  abs(array[ind] - f[i]) $
                  THEN ind++ ELSE check = 0

            ENDWHILE

            IF FINITE(array[ind]) $
               AND FINITE(f[i]) THEN $
               output[i] = ind $
            ELSE output[i] = -1

         ENDFOR

      ENDIF ELSE BEGIN

         sorted_indices_f = sort(f)

         sorted_indices_array = sort(array)
      
         FOR i = 0L, n - 1L DO BEGIN
 
            check = 1

            WHILE check AND (ind NE n_array_1) DO BEGIN
                          
               IF abs(array[sorted_indices_array[ind + 1]] - f[sorted_indices_f[i]]) LE $
                  abs(array[sorted_indices_array[ind]] - f[sorted_indices_f[i]]) $
                  THEN ind++ ELSE check = 0
  
            ENDWHILE
      
            IF FINITE(array[sorted_indices_array[ind]]) $
               AND FINITE(f[sorted_indices_f[i]]) THEN $
               output[sorted_indices_f[i]] = sorted_indices_array[ind] $
            ELSE output[sorted_indices_f[i]] = -1

         ENDFOR

      ENDELSE

      RETURN, output

   ENDELSE

END
