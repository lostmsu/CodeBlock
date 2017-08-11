namespace CodeBlock.BasicObjectModel

type BasicObject =
    struct
        val TypeMethods: nativeptr<nativeint>
    end

open CodeBlock.TypeSystem.Cecil

module private BasicModelData =
    let basicObjectType = typeof<BasicObject>.Definition
    let typeMethodsField = findByName "TypeMethods" basicObjectType.Fields