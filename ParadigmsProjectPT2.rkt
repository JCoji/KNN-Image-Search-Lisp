#lang racket

;Jonathan Cojita - 300283917
;Winona Chung - 300298148

;In: Name of query file, name of directory of dataset images
;Out: Displays 5 closest images and distance to query 
;Desc: Main function
(define (similaritySearch queryHistogramFilename imageDatasetDirectory)
  (display (string-append (string-append "The 5 most similar images to " queryHistogramFilename) " are:\n"))

  (let ((query (cdr (storeFile queryHistogramFilename))))
    (let ((result (sort (compare-all (normalize-histogram query (histogram-sum query)) imageDatasetDirectory (directory-list imageDatasetDirectory)) pair-greater?)))
      (display (k-nearest result))
      (display "\n")
      )
    )  
  )

;In: file name
;Out: A list storing all elements in a .txt file
;Desc: Reads .txt file stores in a list
(define (storeFile filePath)
  (let ((p (open-input-file filePath)))
    (let f ((x (read p))) ; reading from file
      (if (eof-object? x) ; check for eof
          (begin
            (close-input-port p)
            '())
          (cons x (f (read p))))))
  )

;In: A histogram (a list) and a integer representing the sum of all elements of histogram
;Out: An integer representing the normal of inputed histogram
;Desc: Uses normalization algorithm to normalize histogram
(define (normalize-histogram histogram sum)
  (if (null? histogram)
      '()
      (cons (/ (car histogram) sum)
            (normalize-histogram (cdr histogram) sum))))

;In: A histogram (list)
;Out: Integer which is the sum of all elements of list
;Desc: Used as a helper class for the normalize-histogram function
(define (histogram-sum histogram)
  (if (null? histogram)
      0.0
      (+ (car histogram) (histogram-sum (cdr histogram)))
      )
  )

;In: 2 Histograms (query histogram, dataset histogram)
;Out: Integer representing the distance between the 2 histograms
;Desc: Uses comparison algorithm to compare 2 functions. Used as a helper class to compare-all
(define (compare query dataHistogram)
  (if (> (length query) 0 )
    (+ (min (car query) (car dataHistogram)) (compare (cdr query) (cdr dataHistogram)))
    0
    )
  )

;In: query histogram, name of the directory storing the dataset images, a list storing all dataset photo names
;Out: A list of pairs (dataset-image-name, distance from query)
;Desc: Computes distance to query images for all dataset images in given directory
(define (compare-all query folderName dataset)
  (if (null? dataset)
      '()
      (cons (cons (path->string (car dataset)) (compare query (normalize-file folderName dataset))) (compare-all query folderName (cdr dataset)))
      )
  )


;In: Folder directory name, List of files in directory
;Out: A list of normalized histograms
;Desc: A helper function, combines multiple steps to fully normalize dataset histograms from file
(define (normalize-file folderName fileList)
     (let ((cur (cdr(storeFile (string-append (string-append folderName "/")(path->string(car fileList)))))))
       (normalize-histogram cur (histogram-sum cur))
       )
      
  )

;In: 2 pairs
;Out: boolean
;Desc: Helper function, used to sort pairs in main function
(define (pair-greater? p1 p2)
  (> (cdr p1) (cdr p2))
  )

;In: List (used for histograms)
;Out: Returns the 5 first elements of a list
;Desc: Used at very end of computation to return k-nearest neibors (assuming pre-sorted lst)
(define (k-nearest lst)
  (let take ((n 5)
             (lst lst))
    (if (or (null? lst) (= n 0))
        '()
        (cons (car lst) (take (- n 1) (cdr lst)))
        )
    )
  )




;Calls for all query images
(similaritySearch "q00.txt" "imageDataset2_15_20")
(similaritySearch "q01.txt" "imageDataset2_15_20")
(similaritySearch "q02.txt" "imageDataset2_15_20")
(similaritySearch "q03.txt" "imageDataset2_15_20")
(similaritySearch "q04.txt" "imageDataset2_15_20")
(similaritySearch "q05.txt" "imageDataset2_15_20")
(similaritySearch "q06.txt" "imageDataset2_15_20")
(similaritySearch "q07.txt" "imageDataset2_15_20")
(similaritySearch "q08.txt" "imageDataset2_15_20")
(similaritySearch "q09.txt" "imageDataset2_15_20")
(similaritySearch "q10.txt" "imageDataset2_15_20")
(similaritySearch "q11.txt" "imageDataset2_15_20")
(similaritySearch "q12.txt" "imageDataset2_15_20")
(similaritySearch "q13.txt" "imageDataset2_15_20")
(similaritySearch "q14.txt" "imageDataset2_15_20")
(similaritySearch "q15.txt" "imageDataset2_15_20")