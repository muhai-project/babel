(in-package :web-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; Functionality to visualise 2D arrays as grayscale images ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-js 'show-matrix-image "
function drawArrayAsImage(arr, canvasId) {
      const height = arr.length;
      const width = arr[0].length;

      const canvas = document.getElementById(canvasId);

      canvas.width = width;
      canvas.height = height;
      const ctx = canvas.getContext('2d');
      const imageData = ctx.createImageData(width, height);

      let i = 0;
      for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
          const intensity = arr[y][x];
          imageData.data[i++] = intensity; // R
          imageData.data[i++] = intensity; // G
          imageData.data[i++] = intensity; // B
          imageData.data[i++] = 255;       // A (opaque)
        }
      }

      ctx.putImageData(imageData, 0, 0);
    }
")

(define-css 'matrix-image "
.matrix-image {
 image-rendering: pixelated;
 width: 150px; 
 height: 150px;
}
")


#|

(defun add-array-image-to-wi (list)
  (let ((canvas-id (mkstr (make-id "c"))))
    (add-element `((canvas :class "matrix-image" :id ,canvas-id)))
    (add-element `((script) `(format nil "drawArrayAsImage(~a,~a")
                   list canvas-id))))
|#