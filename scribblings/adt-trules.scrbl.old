#lang scribble/manual

@require["util.rkt"
         "adt-utils.rkt"
         scriblib/render-cond
         (for-label (only-meta-in 0 typed/racket))]
@(use-mathjax)

@title[#:style (with-html5 manual-doc-style)
       #:version (version-text)]{Typing rules}

@todo{Should the filter be something else than @${ϵ|ϵ} or is the filter inferred
 via other rules when the ``function'' does not do anything special?}


@$${
 @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o}
 @${Γ ⊢ @ctor[@κ e] : @ctor[@κ τ] ; ϵ|⊥ ; ∅}
 @${@textsc{T-Ctor-Build}}
 ]
}

@${@applyfilter} is defined
in@~cite[#:precision "p. 75" "tobin-hochstadt_typed_2010"].

@htodo{their second (p. 75) definition of applyfilter does not clearly state
 that ϵ in τ_ϵ means the empty path (actually, ϵ means an emtpy
 \overrightarrow{?} for any such sequence. Also, τ_π matches τ, with π = ϵ (and
 therefore τ_ϵ matches τ). So that's how Number|\overline{Number} gets processed
 with the updated applyfilter.}

@$${
 @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o \\
   φ_r
   = @applyfilter(@ctor[@κ ⊤]|\overline{@ctor[@κ ⊤]}, τ, o)}
 @${Γ ⊢ (@ctor-pred[@κ] e) : Boolean ; φ_r ; ∅}
 @${@textsc{T-Ctor-Pred}}
 ]
}

@$${
 @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o \\ τ <: @ctor[@κ @${τ'}] \\
   o_r = @"\\left\\{" \begin{array}{rl}
   @πctor-val(π(x)) @& @textif o = π(x) @nl
   ∅ @& @otherwise
   \end{array}\right. \\
   φ_r
   = @applyfilter(\overline{\#f}_{@πctor-val}|\#f_{@πctor-val},
   τ, o)}
 @${Γ ⊢ (@ctor-val[@κ]\ e) : τ' ; φ_r ; o_r}
 @${@textsc{T-Ctor-Val}}
 ]
}

@$${
 @$inferrule[
 @${@repeated{Γ ⊢ eᵢ : τᵢ ; φᵢ ; oᵢ}}
 @${Γ ⊢ @record[@repeated{@|ɐ|ᵢ = eᵢ}]
   : @record[@repeated{@|ɐ|ᵢ = τᵢ}]; ϵ|⊥ ; ∅}
 @${@textsc{T-Record-Build}}
 ]
}

@$${
 @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o \\
   φ_r = @applyfilter(@record[@repeated{@|ɐ|ᵢ : ⊤}]
   |\overline{@record[@repeated{@|ɐ|ᵢ : ⊤}]}, τ, o)}
 @${Γ ⊢ (@record-pred[@repeated{@|ɐ|ᵢ}] e) : Boolean ; φ_r ; ∅}
 @${@textsc{T-Record-Pred}}
 ]
}

@$${
 @cond-element[[latex @list{\let\savedamp&}] [else ""]]
 @$inferrule[
 @${Γ ⊢ e : τ ; φ ; o \\ τ <: @record[@repeated{@|ɐ|ᵢ : τᵢ}] \\
   o_r = @"\\left\\{" \begin{array}{rl}
   @πɐ{@|ɐ|ⱼ}(π(x)) @& @textif o = π(x) @nl
   ∅ @& @otherwise
   \end{array}\right. \\
   φ_r
   = @applyfilter(\overline{\#f}_{@πɐ{@|ɐ|ⱼ}}|\#f_{@πɐ{@|ɐ|ⱼ}},
   τ, o)}
 @${Γ ⊢ e.@|ɐ|ⱼ : τ' ; φ_r ; o_r}
 @${@textsc{T-Record-GetField}}
 ]
}

@$${
 @$inferrule[
 @${
   Γ ⊢ e_{r} : τ_{r} ; φ_{r} ; o_{r} \\
   τ_{r} <: @record[@repeated{@|ɐ|ᵢ : τ'ᵢ}] \\
   Γ ⊢ e_{v} : τ_{v} ; φ_{v} ; o_{v} \\
   @|ɐ| ∉ \{@|ɐ|ᵢ\}
  }
 @${Γ ⊢ @opwith[@${e_{r}} @|ɐ| @${e_{v}}]
   : @record[@repeated{@|ɐ|ᵢ : τ'ᵢ} @${@|ɐ| : τ_{v}}]
   ; ϵ|⊥ ; ∅}
 @${@textsc{T-Record-With}_1}
 ]
}

@$${
 @$inferrule[
 @${
   Γ ⊢ e_{r} : τ_{r} ; φ_{r} ; o_{r} \\
   τ_{r} <: @record[@repeated{@|ɐ|ᵢ : τ'ᵢ}] \\
   Γ ⊢ e_{v} : τ_{v} ; φ_{v} ; o_{v} \\
   @|ɐ|ⱼ : τ'ⱼ ∈ @repeatset{@|ɐ|ᵢ : τ'ᵢ}
  }
 @${Γ ⊢ @opwith[@${e_{r}} @${@|ɐ|ⱼ} @${e_{v}}]
   : @record[@${@repeatset{@|ɐ|ᵢ : τ'ᵢ} ∖ \{@|ɐ|ⱼ : τ'ⱼ\}} @${@|ɐ|ⱼ : τ_{v}}]
   ; ϵ|⊥ ; ∅}
 @${@textsc{T-Record-With}_2}
 ]
}

@$${
 @$inferrule[
 @${
   Γ ⊢ e_{r} : τ_{r} ; φ_{r} ; o_{r} \\
   τ_{r} <: @record[@repeated{@|ɐ|ᵢ : τ'ᵢ}] \\
   @|ɐ|ⱼ : τ'ⱼ ∈ @repeatset{@|ɐ|ᵢ : τ'ᵢ}
  }
 @${Γ ⊢ @opwithout[@${e_{r}} @|ɐ|]
   : @record[@${@repeatset{@|ɐ|ᵢ : τ'ᵢ} ∖ \{@|ɐ|ⱼ : τ'ⱼ\}}]
   ; ϵ|⊥ ; ∅}
 @${@textsc{T-Record-Without}}
 ]
}
