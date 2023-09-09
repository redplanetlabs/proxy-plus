(ns com.rpl.asm
  (:refer-clojure :exclude
    [cast monitor-enter monitor-exit not pop get-method])
  (:require [clojure.string :as str])
  (:import [rpl.shaded.org.objectweb.asm MethodVisitor
            ClassVisitor ClassWriter
            AnnotationVisitor Label Opcodes Type]
           [rpl.shaded.org.objectweb.asm.util CheckClassAdapter]
           [rpl.shaded.org.objectweb.asm.commons GeneratorAdapter Method]
           [java.io PrintWriter]
           [clojure.lang DynamicClassLoader]
           [java.lang.reflect Field]))



(defn class-name->internal-name [^String name]
  (str/replace name "." "/"))


(defn dynamic-class-loader []
  (DynamicClassLoader.))


(defn define-class [^DynamicClassLoader cl ^String name ^ClassWriter cw]
  (let [klass (.defineClass cl name (.toByteArray cw) nil)]
    (when *compile-files*
      (clojure.lang.Compiler/writeClassFile
        (class-name->internal-name (.getName klass))
        (.toByteArray cw)))
    klass))


;; Constants

(defn- clojurify-name [^String s]
  (str/replace s "_" "-"))

(defn- make-static-constants [prefix ^Class klass]
  (doseq [^Field f (.getFields klass)]
    (let [name (symbol (str prefix (clojurify-name (.getName f))))]
      (if (find-var (symbol (str *ns* "/" name)))
        (throw (ex-info "Var for constant is already bound" {:name name})))
      (intern *ns* name (.get f f)))))


(make-static-constants "O" Opcodes)
(make-static-constants "T" Type)
(make-static-constants "G" GeneratorAdapter)


;; ClassWriter

(def COMPUTE-MAXS ClassWriter/COMPUTE_MAXS)
(def COMPUTE-FRAMES ClassWriter/COMPUTE_FRAMES)

(defn class-writer
  ([] (class-writer 0))
  ([flags] (ClassWriter. flags)))


(defn check-class-adapter [cw]
  (CheckClassAdapter. cw false))

(defn class-writer-auto []
  (class-writer (+ COMPUTE-MAXS COMPUTE-FRAMES)))

(defn visit [^ClassVisitor cw name super-name & interfaces]
  (.visit
    cw
    OV1-7
    (+ OACC-SUPER OACC-PUBLIC)
    name
    nil
    super-name
    (into-array String interfaces)))

(defn visit-interface [^ClassVisitor cw name & interfaces]
  (.visit
    cw
    OV1-7
    (+ OACC-ABSTRACT OACC-PUBLIC OACC-INTERFACE)
    name
    nil
    "java/lang/Object"
    (into-array String interfaces)))

(defn visit-method [^ClassVisitor cw name desc]
  (.visitMethod cw OACC-PUBLIC name desc nil nil))

(defn visit-interface-method [^ClassVisitor cw name desc]
  (.visitMethod cw (+ OACC-PUBLIC OACC-ABSTRACT)
                name desc nil nil))

(defn visit-field [^ClassVisitor cw name desc]
  (.visitField cw OACC-PUBLIC name desc nil nil))

(defn visit-static-field [^ClassVisitor cw name desc value]
  (.visitField cw (+ OACC-STATIC OACC-PUBLIC) name desc nil value))

(defn visit-static-long-field [^ClassVisitor cw name desc ^long value]
  (.visitField cw (+ OACC-STATIC OACC-PUBLIC) name desc nil value))

(defn visit-source [^ClassVisitor cw file debug]
  (.visitSource cw file debug))

(defn cw-visit-end [^ClassVisitor cw]
  (.visitEnd cw))


;; MethodVisitor

(defn visit-code [^MethodVisitor mv]
  (.visitCode mv))

(defn visit-insn
  [^MethodVisitor mv code]
  (.visitInsn mv (int code)))

(defn visit-int-insn
  [^MethodVisitor mv code operand]
  (.visitIntInsn mv (int code) (int operand)))

(defn visit-var-insn
  [^MethodVisitor mv code v]
  (.visitVarInsn mv (int code) (int v)))

(defn visit-type-insn
  [^MethodVisitor mv code type]
  (.visitTypeInsn mv (int code) type))

(defn visit-field-insn [^MethodVisitor mv code owner name desc]
  (.visitFieldInsn mv (int code) owner name desc))

(defn visit-method-insn [^MethodVisitor mv code owner name desc]
  (.visitMethodInsn mv (int code) owner name desc))

(defn visit-jump-insn [^MethodVisitor mv code ^Label l]
  (.visitJumpInsn mv (int code) l))

(defn visit-label [^MethodVisitor mv ^Label l]
  (.visitLabel mv l))

(defn visit-ldc-insn [^MethodVisitor mv c]
  (.visitLdcInsn mv c))

(defn visit-iinc-insn [^MethodVisitor mv v i]
  (.visitIincInsn mv (int v) (int i)))

(defn visit-table-switch-insn
  [^MethodVisitor mv min max ^Label default & labels]
  (.visitTableSwitchInsn
    mv
    (int min)
    (int max)
    default
    (into-array Label labels)))

(defn visit-lookup-switch-insn
  [^MethodVisitor mv ^Label default keys->labels]
  (let [keys (map first keys->labels)
        labels (map second keys->labels)]
    (.visitLookupSwitchInsn
      mv
      default
      (int-array keys)
      (into-array Label labels))))

(defn visit-multi-a-new-array-insn
  [^MethodVisitor mv desc dims]
  (.visitMultiANewArrayInsn mv desc (int dims)))

(defn visit-try-catch-block
  [^MethodVisitor mv ^Label start ^Label end
   ^Label handler type]
  (.visitTryCatchBlock mv start end handler type))

(defn visit-local-variable
  [^MethodVisitor mv name desc
   ^Label start ^Label end idx]
  (.visitLocalVariable mv name desc nil start end idx))

(defn visit-line-number
  [^MethodVisitor mv line ^Label start]
  (.visitLineNumber mv (int line) start))

(defn visit-maxs
  ([^MethodVisitor mv]
   (.visitMaxs mv 0 0))
  ([^MethodVisitor mv max-stack max-locals]
   (.visitMaxs mv (int max-stack) (int max-locals))))

(defn mv-visit-end [^MethodVisitor mv]
  (.visitEnd mv))


;; Types

(defprotocol ASMType
  (asm-type ^Type [this]))

(extend-protocol ASMType
  Class
  (asm-type [c]
    (Type/getType c))
  String
  (asm-type [s]
    (Type/getType s))
  Type
  (asm-type [t]
    t)
  nil
  (asm-type [t]
    (asm-type Object)))

(defn method-descriptor [ret-type & arg-types]
  (let [args ^objects (into-array (map asm-type arg-types))]
    (Type/getMethodDescriptor (asm-type ret-type) args)))

(defn type-class-name [atype]
  (let [^Type t (asm-type atype)]
    (.getClassName t)))


(defn type-internal-name [atype]
  (let [^Type t (asm-type atype)]
    (.getInternalName t)))


(defn type-descriptor [class-or-type]
  (.getDescriptor ^Type (asm-type class-or-type)))

(defn class-name->type-descriptor [^String s]
  (str "L" (class-name->internal-name s) ";"))

(defn op-code [^Type t code]
  (.getOpcode t code))

;; Generator adapter

(defn generator-adapter [^Method m ^ClassWriter cw]
  (let [ret (GeneratorAdapter. OACC-PUBLIC m nil nil cw)]
    (visit-code ret)
    ret))

(defn generator-adapter-static [^Method m ^ClassWriter cw]
  (let [ret (GeneratorAdapter. (+ OACC-PUBLIC OACC-STATIC) m nil nil cw)]
    (visit-code ret)
    ret))


(defn array-length [^GeneratorAdapter ga]
  (.arrayLength ga))

(defn array-load [^GeneratorAdapter ga t]
  (.arrayLoad ga (asm-type t)))

(defn array-store [^GeneratorAdapter ga t]
  (.arrayStore ga (asm-type t)))

(defn box [^GeneratorAdapter ga t]
  (.box ga (asm-type t)))

(defn cast [^GeneratorAdapter ga from-type to-type]
  (.cast ga (asm-type from-type) (asm-type to-type)))

(defn catch-exception
  [^GeneratorAdapter ga ^Label start ^Label end ^Label handler t]
  (visit-try-catch-block
    ga
    start
    end
    handler
    (type-internal-name t)))


(defn check-cast [^GeneratorAdapter ga t]
  (.checkCast ga (asm-type t)))

(defn dup [^GeneratorAdapter ga]
  (.dup ga))

(defn dup2 [^GeneratorAdapter ga]
  (.dup2 ga))

(defn dup2X1 [^GeneratorAdapter ga]
  (.dup2X1 ga))

(defn dup2X2 [^GeneratorAdapter ga]
  (.dup2X2 ga))

(defn dupX1 [^GeneratorAdapter ga]
  (.dupX1 ga))

(defn dupX2 [^GeneratorAdapter ga]
  (.dupX2 ga))

(defn end-method [^GeneratorAdapter ga]
  (.endMethod ga))


(defn get-field [^GeneratorAdapter ga owner-type ^String name t]
  (.getField ga (asm-type owner-type) name (asm-type t)))

(defn get-local-type [^GeneratorAdapter ga ^long local]
  (.getLocalType ga local))

(defn get-static [^GeneratorAdapter ga owner-type ^String name t]
  (.getStatic ga (asm-type owner-type) name (asm-type t)))

(defn goto [^GeneratorAdapter ga ^Label l]
  (.goTo ga l))

(defn ifcmp [^GeneratorAdapter ga t ^long mode ^Label l]
  (.ifCmp ga (asm-type t) mode l))

(defn ificmp [^GeneratorAdapter ga ^long mode ^Label l]
  (.ifICmp ga mode l))

(defn ifnonnull [^GeneratorAdapter ga ^Label l]
  (.ifNonNull ga l))

(defn ifnull [^GeneratorAdapter ga ^Label l]
  (.ifNull ga l))

(defn ifzcmp [^GeneratorAdapter ga ^long mode ^Label l]
  (.ifZCmp ga mode l))

(defn instance-of [^GeneratorAdapter ga t]
  (.instanceOf ga (asm-type t)))

(defn invoke-constructor [^GeneratorAdapter ga t ^Method m]
  (.invokeConstructor ga (asm-type t) m))

(defn invoke-interface [^GeneratorAdapter ga t ^Method m]
  (.invokeInterface ga (asm-type t) m))

(defn invoke-static [^GeneratorAdapter ga t ^Method m]
  (.invokeStatic ga (asm-type t) m))

(defn invoke-virtual [^GeneratorAdapter ga t ^Method m]
  (.invokeVirtual ga (asm-type t) m))

(defn load-arg [^GeneratorAdapter ga ^long arg]
  (.loadArg ga arg))

(defn load-arg-array [^GeneratorAdapter ga]
  (.loadArgArray ga))

(defn load-args
  ([^GeneratorAdapter ga]
   (.loadArgs ga))
  ([^GeneratorAdapter ga ^long arg ^long count]
   (.loadArgs ga arg count)))

(defn load-local
  ([^GeneratorAdapter ga ^long local]
   (.loadLocal ga local))
  ([^GeneratorAdapter ga ^long local t]
   (.loadLocal ga local (asm-type t))))

(defn load-this [^GeneratorAdapter ga]
  (.loadThis ga))

(defn mark
  ([^GeneratorAdapter ga]
   (.mark ga))
  ([^GeneratorAdapter ga ^Label l]
   (.mark ga l)))

(defn math [^GeneratorAdapter ga ^long op t]
  (.math ga op (asm-type t)))

(defn monitor-enter [^GeneratorAdapter ga]
  (.monitorEnter ga))

(defn monitor-exit [^GeneratorAdapter ga]
  (.monitorExit ga))

(defn new-instance [^GeneratorAdapter ga t]
  (.newInstance ga (asm-type t)))

(defn new-label [^GeneratorAdapter ga]
  (.newLabel ga))

(defn new-local [^GeneratorAdapter ga t]
  (.newLocal ga (asm-type t)))

(defn not [^GeneratorAdapter ga]
  (.not ga))

(defn pop [^GeneratorAdapter ga]
  (.pop ga))

(defn pop2 [^GeneratorAdapter ga]
  (.pop2 ga))

(defn push-boolean [^GeneratorAdapter ga b]
  (.push ga (boolean b)))

(defn push-char [^GeneratorAdapter ga v]
  (.push ga (int v))
  (.cast ga TINT-TYPE TCHAR-TYPE))

(defn push-double [^GeneratorAdapter ga ^double d]
  (.push ga d))

(defn push-float [^GeneratorAdapter ga ^double f]
  (.push ga (float f)))

(defn push-int [^GeneratorAdapter ga ^long i]
  (.push ga (int i)))

(defn push-long [^GeneratorAdapter ga ^long l]
  (.push ga l))

(defn push-string [^GeneratorAdapter ga ^String s]
  (.push ga s))

(defn push-null [^GeneratorAdapter ga]
  (push-string ga nil))

(defn push-type [^GeneratorAdapter ga t]
  (.push ga (asm-type t)))

(defn put-field [^GeneratorAdapter ga owner-type ^String name t]
  (.putField ga (asm-type owner-type) name (asm-type t)))

(defn put-static [^GeneratorAdapter ga owner-type ^String name t]
  (.putStatic ga (asm-type owner-type) name (asm-type t)))

(defn ret [^GeneratorAdapter ga ^long local]
  (.ret ga local))

(defn return-value [^GeneratorAdapter ga]
  (.returnValue ga))

(defn store-arg [^GeneratorAdapter ga ^long arg]
  (.storeArg ga arg))

(defn store-local
  ([^GeneratorAdapter ga ^long l]
   (.storeLocal ga l))
  ([^GeneratorAdapter ga ^long l t]
   (.storeLocal ga l (asm-type t))))

(defn swap
  ([^GeneratorAdapter ga]
   (.swap ga))
  ([^GeneratorAdapter ga ^Type prev t]
   (.swap ga prev (asm-type t))))

(defn throw-exception
  ([^GeneratorAdapter ga]
   (.throwException ga))
  ([^GeneratorAdapter ga t ^String msg]
   (.throwException ga (asm-type t) msg)))

(defn new-array
  ([^GeneratorAdapter ga t]
   (.newArray ga (asm-type t)))
  ([^GeneratorAdapter ga n t]
   (push-int ga n)
   (.newArray ga (asm-type t))))

(defn unbox [^GeneratorAdapter ga t]
  (.unbox ga (asm-type t)))

(defn value-of [^GeneratorAdapter ga t]
  (.valueOf ga (asm-type t)))

;; Method

(defn desc->method [^String desc]
  (Method/getMethod desc))

(defn get-method [ret-type n arg-types]
  (desc->method
    (str
      (type-class-name ret-type) " "
      n
      "("
      (str/join "," (mapv type-class-name arg-types))
      ")")))


(defn get-constructor [arg-types]
  (get-method TVOID-TYPE "<init>" arg-types))


(def EMPTY-CONSTRUCTOR-METHOD
  (desc->method "void <init> ()"))


;; Label

(defn label []
  (Label.))
