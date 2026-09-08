! Compatibility types for Aura's small pure-Fortran llama2 port.
!
! The original upstream source was previously ignored by .gitignore, which
! left llama2_mod.f90 unable to compile from a clean checkout.  Keep these
! types deliberately small: llama2_mod owns model dimensions and allocation.
module precision_module
    use iso_fortran_env, only: real32, real64
    implicit none
    integer, parameter, public :: wp = real32
end module precision_module

module weight_module
    use precision_module, only: wp
    implicit none
    private
    public :: TransformerWeights, RunState

    type, public :: TransformerWeights
        real(kind=wp), allocatable :: token_embedding_table(:, :)
        real(kind=wp), allocatable :: rms_att_weight(:, :)
        real(kind=wp), allocatable :: wqkv(:, :, :)
        real(kind=wp), allocatable :: wo(:, :, :)
        real(kind=wp), allocatable :: rms_ffn_weight(:, :)
        real(kind=wp), allocatable :: w13(:, :, :)
        real(kind=wp), allocatable :: w2(:, :, :)
        real(kind=wp), allocatable :: rms_final_weight(:)
        real(kind=wp), allocatable :: wcls(:, :)
    end type TransformerWeights

    type, public :: RunState
        real(kind=wp), allocatable :: att(:, :)
        real(kind=wp), allocatable :: key_cache(:, :, :)
        real(kind=wp), allocatable :: value_cache(:, :, :)
        real(kind=wp) :: times(5) = 0.0_wp
    end type RunState
end module weight_module
