#lang racket
(provide (all-defined-out))
(define t1 (current-inexact-milliseconds))
(define comm_line_args (vector->list (current-command-line-arguments)))
(define (get_name) (list-ref comm_line_args 0))    ; get name of file
(define file_name (get_name))                ; define a variable file_name
(define in_port (open-input-file file_name)) ; open the file

;parameters are the first line of file
(define parameters
  (map (lambda(x) (string->number x))
       (string-split (read-line in_port))))

(define n (list-ref parameters 0))
(define dim (list-ref parameters 1))
(define k (list-ref parameters 2))
(define eps (list-ref parameters 3))
(define minPts (list-ref parameters 4))

;(display parameters)

;this function will traverse the file line by line and insert data point into list
; returns : the list of data points
; list element -> (index data-point)
(define (get_data_points index result)
  (cond
    ((> index n) result)
    (else (get_data_points
           (+ index 1)
           (append result
                   (list (append
                          (list index)
                          (list (map (lambda(x) (string->number x)) (string-split(read-line in_port))))
                          )
                   )
           ))
    ))
)

(define data_points (get_data_points 1 '()))
(define step1 data_points)
;(display data_points)

; this function is passed two data points(each point is a list of values)
; the function returns the sum of squares of difference in each dimension
(define (euclidean_sq_sum data_pt1 data_pt2)
  (cond
  	((null? data_pt1) 0)
  	(else
  		(+  
  			(expt (- (car data_pt1) (car data_pt2)) 2)
  			(euclidean_sq_sum (cdr data_pt1) (cdr data_pt2))
		)	
	)
  )	
)

; this function is passed two indices of points between which distance has to be computed
; returns: the distance between the two points
(define (compute_distance i j)
  (cond
    ((= i j) +inf.0)
    (else (sqrt 
    		(euclidean_sq_sum
                 (list-ref (list-ref data_points (- i 1)) 1)
                 (list-ref (list-ref data_points (- j 1)) 1)
            )
          )
    )
  )
)

; this function is passed two indices i,j(i and j used for iterating matrix) and the matrix which is to be filled
; returns: the similarity matrix
(define (compute_similarity_matrix i j matrix)
  (cond
    ((> i n) matrix)
    ((> j n) (compute_similarity_matrix (+ i 1) 1 matrix))
    ((= j 1) (compute_similarity_matrix 
    			i 
    			(+ j 1)
    			(append 
    				matrix
    				(list (list (list j (compute_distance i j))))
              	)
    		)
	)
    (else (compute_similarity_matrix
    		i
           	(+ j 1)
           	(append 
           		(reverse (cdr (reverse matrix)))
           		(list (append 
           				(car (reverse matrix))
           				(list (list j (compute_distance i j)))
           			  )
           		)
            )
          ))
    )
)

(define similarity_matrix (compute_similarity_matrix 1 1 '()))
;(display similarity_matrix)
(define step2 similarity_matrix)

; this function is passed a list of elements(each element having 2 values)
; returns: a list where each element is the first value of input list element
(define (extract_first_ele lis)
	(map
		(lambda(elem) (list-ref elem 0))
		lis
	)
)
; this function is passed a row of the similarity matrix
; returns a list of indices of the k smallest elements in the row 
(define (get_kmin_elems lis)
	(extract_first_ele
		(take (sort lis (lambda(x y) (< (list-ref x 1) (list-ref y 1)))) k)
	)
)
 
; the function returns the list of indices of knn of all points
(define (get_knn)
	(map
		(lambda(row)
			(sort 
				(get_kmin_elems row) 
				(lambda(x y) (< x y))		
			)
		)
		similarity_matrix
	)
)

(define sparsify_matrix (get_knn))
;(display sparsify_matrix)
(define step3 sparsify_matrix)
;(newline)
; the function checks if x is in list or not
(define (member1 x lis)
	(cond
		((member x lis) #t)
		(else #f)
	)
)

; the function returns the intersection of 2 lists
(define (intersect_lists lis1 lis2)
	(cond
		((null? lis1) '())
		((member1 (car lis1) lis2) 
			(cons 
				(car lis1) 
				(intersect_lists (cdr lis1) lis2)
			)
		)
		(else (intersect_lists (cdr lis1) lis2))
	)
)

; the function returns the weight of edge (p,q)
(define (get_weight p q)
	(length (intersect_lists 
				(list-ref sparsify_matrix (- p 1)) 
				(list-ref sparsify_matrix (- q 1))
			)
	)
)

; this function is passed the point(p) for which adjacency list is to be returned
; the adjacency list having each element as (adjacent_vertex weight)
(define (get_adjacent_vertices p q adj_list_point)
	(cond
		((> q n) adj_list_point)
		((= p q) (get_adjacent_vertices p (+ q 1) adj_list_point))
		((and 
			(member1 p (list-ref sparsify_matrix (- q 1))) 
			(member1 q (list-ref sparsify_matrix (- p 1)))
		 )
			(get_adjacent_vertices 
				p 
				(+ q 1)
				(append adj_list_point (list (list q (get_weight p q))))
			)
		)
		(else (get_adjacent_vertices p (+ q 1) adj_list_point))
  )
)

; this function returns the shared neighbour graph
(define (get_shared_neighbour_graph)
	(map
		(lambda(index)
			(sort 
				(get_adjacent_vertices index 1 '()) 
				(lambda(x y)
					(cond
						((> (list-ref x 1) (list-ref y 1)) #t)
						((= (list-ref x 1) (list-ref y 1)) (< (list-ref x 0) (list-ref y 0)))
						(else #f)
					)
				)
			)
		)
		(range 1 (+ n 1))
	)
)

(define adj_list_graph (get_shared_neighbour_graph))
;(display adj_list_graph)
(define step4 adj_list_graph)
;(newline)
;this function returns the density of point p
(define (density p)
	(length (filter 
				(lambda(edge) (>= (list-ref edge 1) eps)) 
				(list-ref adj_list_graph (- p 1))
			)
	)
)

;this function returns a list of density of points
(define (get_points_density)
	(map
		(lambda(pt_num) (density pt_num))
		(range 1 (+ n 1))
	)
)

(define density_list (get_points_density))
;(display density_list)
(define step5 density_list)
;(newline)

; this function returns a list of core points
(define (get_core_points)
	(filter
		(lambda(point) (>= (list-ref density_list (- point 1)) minPts))
		(range 1 (+ n 1))
	)
)

(define core_points (get_core_points))
(define step6 core_points)
;(display core_points)
;(newline)

; this function returns list of all those core points which are adjacent to a core point
; and weight of the edge between them is >= eps
(define (get_core_adj_list lis res)
	(cond
		((null? lis) res)
		((and (>= (list-ref (car lis) 1) eps) (member1 (list-ref (car lis) 0) core_points))
			(get_core_adj_list (cdr lis) (append res (list (list-ref (car lis) 0))))
		)
		(else (get_core_adj_list (cdr lis) res))
	)
)

; this function returns the adjacency list of subgraph between core points
(define (create_core_graph)
	(map 
		(lambda(core_point) 
			(append 
				(list core_point) 
				(list (sort (get_core_adj_list (list-ref adj_list_graph (- core_point 1)) '()) (lambda(x y) (< x y))))
			)
		) 
		core_points
	)
)

(define core_graph (create_core_graph))
;(display core_graph)

(define num_core_pts (length core_points))

; this function is used to return the index of the element in a lists if the element is present
(define (index_of ele lis index)
	(cond
		((null? lis) -1)
		((= ele (car lis)) index)
		(else (index_of ele (cdr lis) (+ index 1)))
	)
)

; this function takes as arguments, 
;-the current queue(points to be explored)
;-current cluster of the point  
(define (get_cluster_point queue curr_clus)
	(cond
		((null? queue) curr_clus)
		((not (member1 (car queue) curr_clus)) 
			(get_cluster_point
				(append (cdr queue) (list-ref (list-ref core_graph (index_of (car queue) core_points 0)) 1))
				(append curr_clus (list (car queue)))
			)
		)
		(else (get_cluster_point (cdr queue) curr_clus))
	)
)


; this function checks if the point is in any of the current clusters
; returns: if point is present, returns true else false
(define (in_clusters pt list_clusters)
	(cond
		((null? list_clusters) #f)
		((member1 pt (list-ref (car list_clusters) 1)) #t)  ; nice list-ref use here
		(else (in_clusters pt (cdr list_clusters)))
	)
)


;this function returns list of clusters of core points
(define (get_clusters index cluster_number result)
	(cond
		((> index num_core_pts) result)
		((in_clusters (list-ref core_points (- index 1)) result) ; implement in_clusters
			(get_clusters (+ index 1) cluster_number result)
		)
		(else (get_clusters
			  	(+ index 1)
			  	(+ cluster_number 1)
			  	(append 
			  		result 
			  		(list (list 
			  				cluster_number
			  				(sort 
				  				(get_cluster_point
				  			  		(list (list-ref core_points (- index 1)))
				  			  		'()
				  			  	)
			  			  		(lambda(x y) (< x y))
			  			  	)
			  			  )
			  		)
			  	)
			  )
		)
	)
)

(define clusters (get_clusters 1 1 '()))
;(display clusters)
(define step7 clusters)
;(newline)

; this function returns the noise points in the dataset
(define (get_noise_points)
	(filter
		(lambda(x) 
			(and
				(not (member1 x core_points))
				(= (list-ref density_list (- x 1)) 0)
			)
		)
		(range 1 (+ n 1))
	)
)


(define noise_points (get_noise_points))
;(display noise_points)
(define step8 noise_points)
;(newline)

; this function returns the border points in the dataset
(define (get_border_points)
	(filter
		(lambda(x) 
			(and
				(not (member1 x core_points))
				(not (member1 x noise_points))
			)
		)
		(range 1 (+ n 1))
	)
)

(define border_points (get_border_points))
;(display border_points)
(define step9 border_points)
;(newline)

(define num_clusters (length clusters))


; this function returns the cluster number of the cluster to which x belongs
(define (get_cluster_number x index)
	(cond
		((> index num_clusters) -1)
		((member1 x (list-ref (list-ref clusters (- index 1)) 1)) index)
		(else (get_cluster_number x (+ index 1)))
	)
)

(define (get_weights_core pt)
	(map
		(lambda(x)
			(list
				x
				(get_weight pt x)
			)
		)
		core_points
	)
)

(define (border_cluster_mapping)
	(map
		(lambda(pt)
			(get_cluster_number 
				(list-ref 
					(car 
						(sort 
							(get_weights_core pt) 
							(lambda(x y) 
								(cond
									((> (list-ref x 1) (list-ref y 1)) #t)
									((and (= (list-ref x 1) (list-ref y 1)) (< (list-ref x 0) (list-ref y 0))) #t)
									(else #f)
								)
							)
						)
					) 
					0
				)
				1
			)
		)
		border_points
	)
)

(define bcm (border_cluster_mapping))
;(display bcm)

(define num_border_points (length border_points))
(define (get_border_points_of_cluster cluster_number)
	(map 
		(lambda(pt_num) (list-ref border_points (- pt_num 1)))
		(filter
			(lambda(pt) (= (list-ref bcm (- pt 1)) cluster_number))
			(range 1 (+ num_border_points 1))
		)
	)
)

(define (merge_border_core)
	(map
		(lambda(cluster_number)
			(list
				cluster_number
				(sort
					(append
						(list-ref (list-ref clusters (- cluster_number 1)) 1)
						(get_border_points_of_cluster cluster_number)
					)
					(lambda(x y) (< x y))
				)
			)
		)
		(range 1 (+ num_clusters 1))
	)
)


(define final_clusters (merge_border_core))
(define step10 final_clusters)
;(display final_clusters)
(define t2 (current-inexact-milliseconds))
(define diff (- t2 t1))
