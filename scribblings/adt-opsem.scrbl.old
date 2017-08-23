#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Operational Semantics}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${@ctor[@κ v] ↪ @ctor[@κ v]}
 @${@textsc{E-Ctor-Build}}
 ]
}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${(@ctor-pred[@κ] v) ↪ δ(@ctor-pred[@κ], v)}
 @${@textsc{E-Ctor-Pred}}
 ]
}

We extend the @${δ} relation to accept in its first position not only constant
functions (members of @${c}), but also members of families of operators
indexed by a constructor label or a field label, like @${@ctor-pred[@κ]},
@${@ctor-val[@κ]} and @record-pred[@repeated{@|ɐ|ᵢ}]

@$${
 @aligned{
  δ(@ctor-pred[@κ], v) &= \#t &@textif v =  @ctor[@κ @${v'}] \\
  δ(@ctor-pred[@κ], v) &= \#f & \text{ otherwise} \\
 }
}

@todo{Is it really necessary to use a δ-rule for E-Ctor-GetVal ?}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${(@ctor-val[@κ] v) ↪ δ(@ctor-val[@κ], v)}
 @${@textsc{E-Ctor-GetVal}}
 ]
}

@$${
 @aligned{
  δ(@ctor-val[@κ], @ctor[@κ @${v'}]) & = @${v'}
 }
}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${@record[@repeated{@|ɐ|ᵢ = vᵢ}] ↪ @record[@repeated{@|ɐ|ᵢ = vᵢ}]}
 @${@textsc{E-Record-Build}}
 ]
}

@$${
 @$inferrule[
 @${\phantom{x}}
 @${(@record-pred[@repeated{@|ɐ|ᵢ}] v) ↪ δ(@record-pred[@repeated{@|ɐ|ᵢ}], v)}
 @${@textsc{E-Record-Pred}}
 ]
}

@$${
 @aligned{
  δ(@record-pred[@repeated{@|ɐ|ᵢ}], v) &= \#t
  if v = @record[@repeated{@|ɐ|ⱼ = vⱼ}] ∧ @repeatset{@|ɐ|ⱼ} = @repeatset{@|ɐ|ᵢ}
  \\
  δ(@record-pred[@repeated{@|ɐ|ᵢ}], v) &= \#f otherwise
  }
}

@$${
 @$inferrule[
 @${@|ɐ|' ∈ @repeatset{@|ɐ|ᵢ} \\ @|ɐ|' = @|ɐ|ⱼ}
 @${@record[@repeated{@|ɐ|ᵢ = vᵢ}].@|ɐ|' ↪ vⱼ}
 @${@textsc{E-Record-GetField}}
 ]
}


@todo{This ∖ does not make sense because we remove the label @|ɐ|' from a set of
 label+value tuples. We must define a separate mathematical operator for removal
 of a label+value tuple from a set based on the label.}
@$${
 @$inferrule[
 @${@|ɐ|ⱼ ∈ @repeatset{@|ɐ|ᵢ}}
 @${@opwith[@record[@repeated{@|ɐ|ᵢ = vᵢ}] @${@|ɐ|ⱼ} @${v'}]
   ↪ @record[@${@repeatset{@|ɐ|ᵢ = vᵢ} ∖ \{@|ɐ|ⱼ = vⱼ\}}
             @${\quad @|ɐ|ⱼ = v'}] \\
  (@|ɐ|ⱼ = vⱼ) ∈ @repeatset{@|ɐ|ᵢ = vᵢ}}
 @${@textsc{E-Record-With}_1}
 ]
}

@todo{what to do with the = sign? The a = v sign is syntactical, but could
 easily be understood as a meta comparison, instead of indicating the
 association between the field and the value.}

@$${
 @$inferrule[
 @${@|ɐ|' ∉ @repeatset{@|ɐ|ᵢ}}
 @${@opwith[@record[@repeated{@|ɐ|ᵢ = vᵢ}] @${@|ɐ|'} @${v'}]
   ↪ @record[@repeatset{@|ɐ|ᵢ = vᵢ} @${\quad @${@|ɐ|'} = v'}]}
 @${@textsc{E-Record-With}_2}
 ]
}

@$${
 @$inferrule[
 @${@|ɐ|ⱼ ∈ @repeatset{@|ɐ|ᵢ}}
 @${@opwithout[@record[@repeated{@|ɐ|ᵢ = vᵢ}] @${@|ɐ|ⱼ}]
   ↪ @record[@${@repeatset{@|ɐ|ᵢ = vᵢ} ∖ \{@|ɐ|ⱼ = vⱼ\}}] \\
  (@|ɐ|ⱼ = vⱼ) ∈ @repeatset{@|ɐ|ᵢ = vᵢ}}
 @${@textsc{E-Record-Without}}
 ]
}