;(ql:quickload :qc)

(in-package :qc)

(defclass country ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type (string) :initarg :name :accessor country-name)
   (population :col-type bigint :initarg :population :accessor country-population)
   (size :col-type integer :initarg :size :accessor country-size)
   (density :col-type integer :initarg :density :accessor country-density)
   (continentid :col-type integer :col-references ((continent id))  :initarg :continent-id :accessor continent-id))
  (:documentation "Dao class for a country record from database.")
  (:metaclass dao-class)
  (:table-name country))

(defclass continent ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor continent-name)
   (population :col-type bigint :initarg :population :accessor continent-population)
   (size :col-type integer :initarg :size :accessor continent-size)
   (density :col-type integer :initarg :density :accessor continent-density))
  (:documentation "Dao class for a continent record from database")
  (:metaclass dao-class)
  (:table-name continent))

(defclass city ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor city-name)
   (population :col-type bigint :initarg population :accessor city-population)
   (size :col-type integer :initarg size :accessor city-size)
   (isprimary :col-type boolean :initarg is-primary :accessor city-is-primary)
   (iscapital :col-type boolean :initarg is-capital :accessor city-is-capital)
   (countryid :col-type integer :col-references ((country id)) :initarg :country-id :accessor country-id))
  (:documentation "Dao class for a city record from database")
  (:metaclass dao-class)
  (:table-name city))

(defclass road ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor road-name)
   (size :col-type integer :initarg :size :accessor road-size)
   (speedaverage :col-type integer :initarg :speedaverage :accessor road-speed-average)
   (countryid :col-type integer :col-references ((country id)) :initarg :country-id :accessor country-id))
  (:documentation "Dao class for a road record from database")
  (:metaclass dao-class)
  (:table-name road))

(defclass river ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor  river-name)
   (size :col-type integer :initarg :size :accessor river-size)
   (flow :col-type integer :initarg :flow :accessor river-flow))
  (:documentation "Dao class for a river record from database")
  (:metaclass dao-class)
  (:table-name river))

;(defclass country-river ()
;  ((countryid :col-type integer :primary-key t :col-reference ((country id)) :initarg :country-id :accessor country-id)
;   (riverid :col-type integer :primary-key t :col-reference ((river id)) :initarg :river-id :accessor river-id))
;  (:documentation "Dao class for a splitting table record between River table and Country table from database")
;  (:metaclass dao-class)
;  (:table-name country_river))