data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
zero + b = b
suc a + b = suc (a + b)
infix 100 _+_

data _≡_ : ℕ → ℕ → Set where
  refl : {n : ℕ} → n ≡ n

sym : {n m : ℕ} → n ≡ m → m ≡ n
sym refl = refl

trans : {m n o : ℕ} → m ≡ n → n ≡ o → m ≡ o
trans refl p₂ = p₂

suc-inj : {m n : ℕ} → suc m ≡ suc n → m ≡ n
suc-inj refl = refl

data ⊥ : Set where

¬ : Set → Set
¬ A = A → ⊥

_≢_ : ℕ → ℕ → Set
m ≢ n = ¬(m ≡ n)

zero-img : ∀ {m} → suc m ≢ 0
zero-img ()

induction : (P : ℕ → Set) → P 0 → (∀ {n} → P n → P (suc n)) → (∀ m → P m)
induction pred base hypo zero = base
induction pred base hypo (suc m) = hypo (induction pred base hypo m)

cong : ∀ {m n} → (f : ℕ → ℕ) → m ≡ n → f m ≡ f n
cong f refl = refl

assoc : ∀ m n o → m + (n + o) ≡ (m + n) + o
assoc zero n o = refl
assoc (suc m) n o = cong suc (assoc m n o)

n+zero : ∀ n → n ≡ n + 0
n+zero zero = refl
n+zero (suc n) = cong suc (n+zero n)

suc+ : ∀ m n → suc (n + m) ≡ (n + suc m)
suc+ m zero = refl
suc+ m (suc n) = cong suc (suc+ m n)

comm : ∀ m n → m + n ≡ n + m
comm zero n = n+zero n
comm (suc m) n = trans (cong suc (comm m n)) (suc+ m n)


-- Where to go from here?
-- Agda:
--   • Aaron Stump - Verified Functional Programming in Agda, https://svn.divms.uiowa.edu/repos/clc/projects/agda/book/book.pdf
--   • Conor McBride - Dependently Typed Metaprogramming (in Agda), http://cs.ioc.ee/ewscs/2014/mcbride/mcbride-deptypedmetaprog.pdf
-- Type theory:
--   • So you want to learn type theory, http://purelytheoretical.com/sywtltt.html
