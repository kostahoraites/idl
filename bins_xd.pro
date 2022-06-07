; average input DATA into an arbitrary X-dimensional grid
; this algorithm is independent of the coordinate system used
; (coords and vargrid_lims must be specified in that coordinate system)
;
; INPUTS: (note X is the number of dimensions, n is the number of data to be binned and averaged)
; data: a 1D array of size [n], representing the data that will be averaged into bins
; coords: an array of size [n, X], specifying the positions of the data to be averaged
; vargrid_lims: an array of size [2, X], specifying the min/max for each of the n coordinates, of the averaging grid
; vargrid_nbins: an array of size [X], specifying the number of bins (>0), for each of the X dimensions, that will make up the averaging grid
; 
; HISTORY
;
; 10/14/2019: bins_xd.pro developed as a generalized version of bins.pro, for data sets in arbitrary # of dimensions
;
; EXAMPLES
;
;   n = 1000000L
;   ndims = 3
;   coords = randomu(seed, n* ndims)
;   coords = reform(coords, n, ndims)
;   coords[*,0]*=10d 
;   coords[*,1]*=5d 
;   data = randomu(seed, n) * 100
;   vargrid_lims = [[0,10], [0,5], [0,1]]
;   vargrid_nbins = [10, 10, 10]
;   struc = bins_xd(data, coords, vargrid_lims, vargrid_nbins)
;
;   device, decompose = 0
;   loadct2, 27
;   specplotk, struc.axes[0], struc.axes[1], struc.ave[*,*,3]
;
; TO-DO: test against results from plot_2dhist.pro and/or bins.pro
; 

FUNCTION array_index, indices, array

   ; helper function takes input array subscripts and that is being referenced (used to calculate array_dims),
   ; to convert the array subscripts into a single number that references the same element
   ; (opposite operation of IDL's array_indices.pro)
   
   array_dims = size(array, /dimensions)
   index = 0L
   FOR i = 0L, n_elements(array_dims) -1L DO BEGIN
      IF i EQ 0 THEN BEGIN
         index += indices[0]
      ENDIF ELSE BEGIN
         index += indices[i] * long(product(array_dims[0L:i-1L]))
      ENDELSE
   ENDFOR

   RETURN, index

END

FUNCTION bins_xd, data, coords, vargrid_lims, vargrid_nbins, data_min = data_min, data_max = data_max

   IF not(KEYWORD_SET(data_min)) then data_min = -!values.d_infinity
   IF not(KEYWORD_SET(data_max)) then data_max = !values.d_infinity

   ndims = n_elements(vargrid_nbins)
   ndata = n_elements(data)

      ; initialize an array to average data into, as well as arrays to store the counts, grid axes, etc.

   data_ave = dblarr(vargrid_nbins)
   counts = dblarr(vargrid_nbins)
   bin_index = coords * 0d
   axes = list()

      ; index which bin of data_ave each data point falls into

   FOR i=0L, ndims - 1L DO BEGIN

;      delta = (vargrid_lims[1, i] - vargrid_lims[0, i]) / (vargrid_nbins[i] - 1d)   ; is the -1 in denominator correct?
;      bin_index[*, i] = round( (coords[*, i] - vargrid_lims[0, i]) / delta, /L64 ) 
      delta = (vargrid_lims[1, i] - vargrid_lims[0, i]) / double(vargrid_nbins[i])
      bin_index[*, i] = floor( (coords[*, i] - vargrid_lims[0, i]) / delta, /L64 ) 
      axes.add, genarr( vargrid_nbins[i], double(vargrid_lims[0,i]), double(vargrid_lims[1,i])-delta )

   ENDFOR   

      ; average the data, calculate counts, standard deviation of the mean etc.

   FOR i = 0L, ndata-1L DO BEGIN

      test = where(bin_index[i,*] LT 0 OR bin_index[i,*] GE vargrid_nbins, ntest)  ; check to make indices fall within the prescribed ranges
;      IF ntest eq 0 THEN BEGIN
      IF ntest eq 0 AND data[i] LT data_max and data[i] GT data_min THEN BEGIN
         ind = array_index(reform(bin_index[i, *]), data_ave)
         IF finite(data[i]) eq 1 THEN BEGIN
            data_ave[ind] += data[i]
            counts[ind]++
         ENDIF
      ENDIF

   ENDFOR

   data_ave = data_ave / counts   ; NOTE: will very likely be dividing by 0 counts in many bins
   ind0 = where(counts eq 0, nind0)
   IF nind0 gt 0 THEN data_ave[ind0] = !values.f_nan

   RETURN, {ave:data_ave, counts:counts, axes:axes}

END


