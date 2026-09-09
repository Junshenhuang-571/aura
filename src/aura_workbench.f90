! aura_workbench.f90 — dependency-free computational-physics project manifest.
!
! The manifest is deliberately a small TOML-like format rather than a second
! build system.  Aura reads it to populate GUI controls and delegates actual
! compilation, scheduling, and monitoring to commands supplied by the user.
module aura_workbench
    use iso_fortran_env, only: i4 => int32
    implicit none
    private

    integer, parameter, public :: WB_MAX_TEMPLATES = 32
    integer, parameter, public :: WB_MAX_SWEEPS = 32
    integer, parameter, public :: WB_MAX_VALUES = 256

    type, public :: wb_template
        character(len=:), allocatable :: name, source_dir, main
        character(len=:), allocatable :: dependencies
        character(len=:), allocatable :: build, run, test
    end type

    type, public :: wb_sweep
        character(len=:), allocatable :: name
        character(len=256), allocatable :: values(:)
        integer :: n_values = 0
    end type

    type, public :: wb_run_record
        character(len=:), allocatable :: id, state, command, cwd, params
        character(len=:), allocatable :: stdout_path, stderr_path, result_path
        integer :: exit_code = -1
    end type

    type, public :: wb_manifest
        character(len=:), allocatable :: version, name, root, default_template
        character(len=:), allocatable :: compiler, compiler_flags
        character(len=:), allocatable :: build_command, run_command, test_command
        logical :: mpi_enabled = .false.
        character(len=:), allocatable :: mpi_launcher
        integer :: mpi_ranks = 1
        logical :: openmp_enabled = .false.
        integer :: openmp_threads = 1
        character(len=:), allocatable :: openmp_flags
        character(len=:), allocatable :: scheduler_kind, scheduler_command
        character(len=:), allocatable :: scheduler_partition
        character(len=:), allocatable :: scheduler_account
        character(len=:), allocatable :: scheduler_time
        integer :: scheduler_cpus = 1
        character(len=:), allocatable :: tracking_directory, monitor_command
        character(len=:), allocatable :: result_format, visualize_command, browse_command
        character(len=:), allocatable :: convergence_command
        type(wb_template), allocatable :: templates(:)
        integer :: n_templates = 0
        type(wb_sweep), allocatable :: sweeps(:)
        integer :: n_sweeps = 0
    contains
        procedure :: defaults => wb_defaults
        procedure :: load => wb_load
        procedure :: save => wb_save
        procedure :: validate => wb_validate
        procedure :: command => wb_command
        procedure :: execute => wb_execute
        procedure :: sweep_count => wb_sweep_count
        procedure :: sweep_point => wb_sweep_point
        procedure :: sweep_command => wb_sweep_command
        procedure :: start_run => wb_start_run
        procedure :: finish_run => wb_finish_run
        procedure :: read_run => wb_read_run
    end type

contains

    subroutine wb_defaults(self)
        class(wb_manifest), intent(inout) :: self
        self%version = '1'
        self%name = 'fortran-physics-project'
        self%root = '.'
        self%default_template = 'default'
        self%compiler = 'gfortran'
        self%compiler_flags = '-std=f2018 -O2'
        self%build_command = '{compiler} {flags} {openmp_flags} {source_dir}/*.f90 -o build/{name}'
        self%run_command = './build/{name}'
        self%test_command = ''
        self%mpi_enabled = .false.
        self%mpi_launcher = 'mpiexec'
        self%mpi_ranks = 1
        self%openmp_enabled = .false.
        self%openmp_threads = 1
        self%openmp_flags = '-fopenmp'
        self%scheduler_kind = 'local'
        self%scheduler_command = 'sbatch'
        self%scheduler_partition = ''
        self%scheduler_account = ''
        self%scheduler_time = ''
        self%scheduler_cpus = 1
        self%tracking_directory = '.aura/runs'
        self%monitor_command = ''
        self%result_format = 'csv'
        self%visualize_command = ''
        self%browse_command = ''
        self%convergence_command = ''
        self%n_templates = 0
        self%n_sweeps = 0
        if (allocated(self%templates)) deallocate(self%templates)
        if (allocated(self%sweeps)) deallocate(self%sweeps)
        allocate(self%templates(WB_MAX_TEMPLATES), self%sweeps(WB_MAX_SWEEPS))
        call add_template(self, 'default')
    end subroutine

    subroutine wb_load(self, path, ok, message)
        class(wb_manifest), intent(inout) :: self
        character(len=*), intent(in) :: path
        logical, intent(out), optional :: ok
        character(len=:), allocatable, intent(out), optional :: message
        integer :: u, ios, p, eq, current_template, current_sweep
        character(len=2048) :: line, section, key, value
        logical :: good
        character(len=:), allocatable :: why

        call self%defaults()
        good = .true.; why = ''
        current_template = 0; current_sweep = 0
        open (newunit=u, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            good = .false.; why = 'manifest not found: '//trim(path)
        else
            do
                read (u, '(A)', iostat=ios) line
                if (ios /= 0) exit
                line = strip_comment(trim(line))
                if (len_trim(line) == 0) cycle
                if (line(1:1) == '[') then
                    p = index(line, ']')
                    if (p <= 2) cycle
                    section = lower(adjustl(line(2:p-1)))
                    current_template = 0; current_sweep = 0
                    if (starts_with(section, 'templates.')) then
                        call add_template(self, unquote(section(11:)))
                        current_template = self%n_templates
                    else if (starts_with(section, 'sweep.')) then
                        call add_sweep(self, unquote(section(7:)))
                        current_sweep = self%n_sweeps
                    end if
                    cycle
                end if
                eq = index(line, '=')
                if (eq <= 1) cycle
                key = lower(trim(line(:eq-1)))
                value = trim(line(eq+1:))
                if (current_template > 0) then
                    call set_template_key(self%templates(current_template), key, value)
                else if (current_sweep > 0) then
                    call set_sweep_key(self%sweeps(current_sweep), key, value)
                else
                    call set_manifest_key(self, section, key, value)
                end if
            end do
            close (u)
            call self%validate(good, why)
        end if
        if (present(ok)) ok = good
        if (present(message)) message = why
    end subroutine

    subroutine wb_save(self, path, ok)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: path
        logical, intent(out), optional :: ok
        integer :: u, ios, i, j
        logical :: good
        character(len=256) :: val

        call make_parent_directory(path)
        open (newunit=u, file=trim(path), status='replace', action='write', iostat=ios)
        good = ios == 0
        if (good) then
            write (u, '(A)') '[project]'
            call put(u, 'version', self%version); call put(u, 'name', self%name)
            call put(u, 'root', self%root); call put(u, 'default-template', self%default_template)
            write (u, '(A)') ''; write (u, '(A)') '[toolchain]'
            call put(u, 'compiler', self%compiler); call put(u, 'flags', self%compiler_flags)
            write (u, '(A)') ''; write (u, '(A)') '[mpi]'
            call put_bool(u, 'enabled', self%mpi_enabled)
            call put(u, 'launcher', self%mpi_launcher); call put_int(u, 'ranks', self%mpi_ranks)
            write (u, '(A)') ''; write (u, '(A)') '[openmp]'
            call put_bool(u, 'enabled', self%openmp_enabled)
            call put_int(u, 'threads', self%openmp_threads); call put(u, 'flags', self%openmp_flags)
            write (u, '(A)') ''; write (u, '(A)') '[scheduler]'
            call put(u, 'kind', self%scheduler_kind); call put(u, 'command', self%scheduler_command)
            call put(u, 'partition', self%scheduler_partition)
            call put(u, 'account', self%scheduler_account); call put(u, 'time', self%scheduler_time)
            call put_int(u, 'cpus-per-task', self%scheduler_cpus)
            write (u, '(A)') ''; write (u, '(A)') '[commands]'
            call put(u, 'build', self%build_command); call put(u, 'run', self%run_command)
            call put(u, 'test', self%test_command)
            write (u, '(A)') ''; write (u, '(A)') '[tracking]'
            call put(u, 'directory', self%tracking_directory)
            call put(u, 'monitor-command', self%monitor_command)
            write (u, '(A)') ''; write (u, '(A)') '[results]'
            call put(u, 'format', self%result_format)
            call put(u, 'visualize-command', self%visualize_command)
            call put(u, 'browse-command', self%browse_command)
            call put(u, 'convergence-command', self%convergence_command)
            do i = 1, self%n_templates
                write (u, '(A)') ''; write (u, '(A)') '[templates.'//trim(self%templates(i)%name)//']'
                call put(u, 'source-dir', self%templates(i)%source_dir)
                call put(u, 'main', self%templates(i)%main)
                call put(u, 'dependencies', self%templates(i)%dependencies)
                call put(u, 'build', self%templates(i)%build)
                call put(u, 'run', self%templates(i)%run); call put(u, 'test', self%templates(i)%test)
            end do
            do i = 1, self%n_sweeps
                write (u, '(A)') ''; write (u, '(A)') '[sweep.'//trim(self%sweeps(i)%name)//']'
                val = ''
                do j = 1, self%sweeps(i)%n_values
                    if (j > 1) val = trim(val)//','
                    val = trim(val)//trim(self%sweeps(i)%values(j))
                end do
                call put(u, 'values', val)
            end do
            close (u)
        end if
        if (present(ok)) ok = good
    contains
        subroutine put(unit, key, value)
            integer, intent(in) :: unit
            character(len=*), intent(in) :: key, value
            write (unit, '(A)') trim(key)//' = "'//toml_escape(value)//'"'
        end subroutine
        subroutine put_bool(unit, key, value)
            integer, intent(in) :: unit
            character(len=*), intent(in) :: key
            logical, intent(in) :: value
            write (unit, '(A,L1)') trim(key)//' = ', value
        end subroutine
        subroutine put_int(unit, key, value)
            integer, intent(in) :: unit, value
            character(len=*), intent(in) :: key
            write (unit, '(A,I0)') trim(key)//' = ', value
        end subroutine
    end subroutine

    subroutine wb_validate(self, ok, message)
        class(wb_manifest), intent(in) :: self
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out), optional :: message
        character(len=:), allocatable :: why
        ok = .true.; why = ''
        if (len_trim(self%version) == 0) then
            ok = .false.; why = 'project.version must not be empty'
        else if (len_trim(self%name) == 0) then
            ok = .false.; why = 'project.name must not be empty'
        else if (len_trim(self%root) == 0) then
            ok = .false.; why = 'project.root must not be empty'
        else if (len_trim(self%compiler) == 0) then
            ok = .false.; why = 'toolchain.compiler must not be empty'
        else if (self%n_templates < 1) then
            ok = .false.; why = 'at least one template is required'
        else if (find_template(self, self%default_template) == 0) then
            ok = .false.; why = 'project.default-template must name a template'
        else if (self%mpi_ranks < 1) then
            ok = .false.; why = 'mpi.ranks must be positive'
        else if (self%openmp_threads < 1) then
            ok = .false.; why = 'openmp.threads must be positive'
        else if (self%scheduler_cpus < 1) then
            ok = .false.; why = 'scheduler.cpus-per-task must be positive'
        else if (self%scheduler_kind /= 'local' .and. self%scheduler_kind /= 'slurm') then
            ok = .false.; why = 'scheduler.kind must be local or slurm'
        else if (self%scheduler_kind == 'slurm' .and. len_trim(self%scheduler_command) == 0) then
            ok = .false.; why = 'scheduler.command is required for slurm'
        else if (len_trim(self%tracking_directory) == 0) then
            ok = .false.; why = 'tracking.directory must not be empty'
        else if (len_trim(self%build_command) == 0 .or. len_trim(self%run_command) == 0) then
            ok = .false.; why = 'commands.build and commands.run are required'
        else
            call validate_children(self, ok, why)
        end if
        if (present(message)) message = why
    contains
        subroutine validate_children(manifest, valid, reason)
            class(wb_manifest), intent(in) :: manifest
            logical, intent(out) :: valid
            character(len=:), allocatable, intent(out) :: reason
            integer :: i
            valid = .true.; reason = ''
            do i = 1, manifest%n_templates
                if (len_trim(manifest%templates(i)%name) == 0) then
                    valid = .false.; reason = 'template name must not be empty'; return
                end if
                if (len_trim(manifest%templates(i)%source_dir) == 0) then
                    valid = .false.; reason = 'template.source-dir must not be empty'; return
                end if
            end do
            do i = 1, manifest%n_sweeps
                if (len_trim(manifest%sweeps(i)%name) == 0) then
                    valid = .false.; reason = 'sweep name must not be empty'; return
                end if
                if (manifest%sweeps(i)%n_values < 1) then
                    valid = .false.; reason = 'sweep.'//trim(manifest%sweeps(i)%name)//'.values must not be empty'; return
                end if
            end do
        end subroutine
    end subroutine

    function wb_command(self, stage, template_name) result(command)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: stage
        character(len=*), intent(in), optional :: template_name
        character(len=:), allocatable :: command, raw, tname, mpi_prefix, omp_flags
        integer :: i
        tname = self%default_template
        if (present(template_name)) then
            if (len_trim(template_name) > 0) tname = trim(template_name)
        end if
        i = find_template(self, tname)
        select case (lower(trim(stage)))
        case ('build')
            raw = self%build_command
            if (i > 0 .and. allocated(self%templates(i)%build) .and. &
                len_trim(self%templates(i)%build) > 0) raw = self%templates(i)%build
        case ('run')
            raw = self%run_command
            if (i > 0 .and. allocated(self%templates(i)%run) .and. len_trim(self%templates(i)%run) > 0) raw = self%templates(i)%run
        case ('test')
            raw = self%test_command
            if (i > 0 .and. allocated(self%templates(i)%test) .and. &
                len_trim(self%templates(i)%test) > 0) raw = self%templates(i)%test
        case ('visualize')
            raw = self%visualize_command
        case ('browse')
            raw = self%browse_command
        case ('monitor')
            raw = self%convergence_command
        case default
            command = ''; return
        end select
        command = raw
        call replace(command, '{name}', self%name)
        call replace(command, '{root}', self%root)
        call replace(command, '{compiler}', self%compiler)
        call replace(command, '{flags}', self%compiler_flags)
        omp_flags = ''
        if (self%openmp_enabled) omp_flags = self%openmp_flags
        call replace(command, '{openmp_flags}', omp_flags)
        call replace(command, '{omp_threads}', int_string(self%openmp_threads))
        if (i > 0) then
            call replace(command, '{source_dir}', self%templates(i)%source_dir)
            call replace(command, '{main}', self%templates(i)%main)
            call replace(command, '{dependencies}', self%templates(i)%dependencies)
        else
            call replace(command, '{source_dir}', self%root//'/src')
            call replace(command, '{main}', '')
            call replace(command, '{dependencies}', '')
        end if
            call replace(command, '{result_format}', self%result_format)
            call replace(command, '{tracking_directory}', self%tracking_directory)
        mpi_prefix = ''
        if (self%mpi_enabled .and. lower(trim(stage)) == 'run') then
            mpi_prefix = trim(self%mpi_launcher)//' -n '//int_string(self%mpi_ranks)//' '
        end if
        call replace(command, '{mpi_prefix}', mpi_prefix)
        if (self%openmp_enabled .and. lower(trim(stage)) == 'run') then
            ! The command can opt into a portable launcher-specific setting.
            call replace(command, '{omp_prefix}', 'OMP_NUM_THREADS='//int_string(self%openmp_threads)//' ')
        else
            call replace(command, '{omp_prefix}', '')
        end if
        command = adjustl(trim(command))
        if (lower(trim(stage)) == 'run' .and. self%scheduler_kind == 'slurm') then
            command = slurm_command(self, command)
        end if
    end function

    subroutine wb_execute(self, stage, template_name, exit_code, command)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: stage
        character(len=*), intent(in), optional :: template_name
        integer, intent(out) :: exit_code
        character(len=:), allocatable, intent(out), optional :: command
        character(len=:), allocatable :: cmd
        integer :: cmdstat
        cmd = self%command(stage, template_name)
        if (present(command)) command = cmd
        if (len_trim(cmd) == 0) then
            exit_code = 127
            return
        end if
        call execute_command_line(cmd, wait=.true., exitstat=exit_code, cmdstat=cmdstat)
        if (cmdstat /= 0 .and. exit_code == 0) exit_code = cmdstat
    end subroutine

    function wb_sweep_count(self) result(n)
        class(wb_manifest), intent(in) :: self
        integer :: n, i
        n = 1
        do i = 1, self%n_sweeps
            n = n * max(1, self%sweeps(i)%n_values)
        end do
    end function

    subroutine wb_sweep_point(self, ordinal, names, values, ok)
        class(wb_manifest), intent(in) :: self
        integer, intent(in) :: ordinal
        character(len=64), allocatable, intent(out) :: names(:)
        character(len=256), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        integer :: i, index_value, stride, n
        allocate(names(self%n_sweeps), values(self%n_sweeps))
        ok = ordinal >= 1 .and. ordinal <= self%sweep_count()
        if (.not. ok) return
        n = ordinal - 1
        do i = 1, self%n_sweeps
            names(i) = self%sweeps(i)%name
            stride = 1
            if (i > 1) stride = product(max(1, self%sweeps(1:i-1)%n_values))
            index_value = mod(n / stride, max(1, self%sweeps(i)%n_values)) + 1
            values(i) = self%sweeps(i)%values(index_value)
        end do
    end subroutine

    function wb_sweep_command(self, ordinal, template_name) result(command)
        class(wb_manifest), intent(in) :: self
        integer, intent(in) :: ordinal
        character(len=*), intent(in), optional :: template_name
        character(len=:), allocatable :: command
        character(len=64), allocatable :: names(:)
        character(len=256), allocatable :: values(:)
        logical :: ok
        integer :: i
        command = self%command('run', template_name)
        call self%sweep_point(ordinal, names, values, ok)
        if (.not. ok) then
            command = ''
            return
        end if
        do i = 1, size(names)
            call replace(command, '{'//trim(names(i))//'}', trim(values(i)))
        end do
    end function

    subroutine wb_start_run(self, run_id, command, record, ok, cwd, params)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: run_id, command
        type(wb_run_record), intent(out) :: record
        logical, intent(out), optional :: ok
        character(len=*), intent(in), optional :: cwd, params
        character(len=:), allocatable :: dir
        record%id = trim(run_id); record%state = 'running'; record%command = trim(command)
        record%cwd = trim(self%root); record%params = ''
        if (present(cwd)) record%cwd = trim(cwd)
        if (present(params)) record%params = trim(params)
        dir = trim(self%tracking_directory)//'/'//trim(run_id)
        record%result_path = dir
        record%stdout_path = trim(dir)//'/stdout.log'
        record%stderr_path = trim(dir)//'/stderr.log'
        record%exit_code = -1
        call make_directory(dir)
        call write_run_record(self, record, ok)
    end subroutine

    subroutine wb_finish_run(self, run_id, exit_code, state, result_file, ok)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: run_id
        integer, intent(in) :: exit_code
        character(len=*), intent(in), optional :: state, result_file
        logical, intent(out), optional :: ok
        type(wb_run_record) :: record
        logical :: read_ok
        character(len=:), allocatable :: final_state
        call self%read_run(run_id, record, read_ok)
        if (.not. read_ok) then
            record%id = trim(run_id); record%command = ''; record%cwd = trim(self%root)
            record%params = ''; record%result_path = trim(self%tracking_directory)//'/'//trim(run_id)
            record%stdout_path = trim(record%result_path)//'/stdout.log'
            record%stderr_path = trim(record%result_path)//'/stderr.log'
        end if
        if (present(state)) then
            final_state = trim(state)
        else if (exit_code == 0) then
            final_state = 'completed'
        else
            final_state = 'failed'
        end if
        record%id = trim(run_id); record%state = final_state; record%exit_code = exit_code
        if (present(result_file)) record%result_path = trim(result_file)
        call write_run_record(self, record, ok)
    end subroutine

    subroutine wb_read_run(self, run_id, record, ok)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: run_id
        type(wb_run_record), intent(out) :: record
        logical, intent(out) :: ok
        integer :: u, ios, eq
        character(len=1024) :: line, key, value
        record%id = trim(run_id); record%state = ''; record%command = ''; record%cwd = ''
        record%params = ''; record%stdout_path = ''; record%stderr_path = ''; record%result_path = ''
        record%exit_code = -1; ok = .false.
        open (newunit=u, file=trim(self%tracking_directory)//'/'//trim(run_id)//'/status.toml', &
              status='old', action='read', iostat=ios)
        if (ios /= 0) return
        ok = .true.
        do
            read (u, '(A)', iostat=ios) line
            if (ios /= 0) exit
            eq = index(line, '=')
            if (eq <= 1) cycle
            key = lower(trim(line(:eq-1))); value = trim(line(eq+1:))
            select case (trim(key))
            case ('id'); record%id = unquote(value)
            case ('state'); record%state = unquote(value)
            case ('command'); record%command = unquote(value)
            case ('cwd'); record%cwd = unquote(value)
            case ('params'); record%params = unquote(value)
            case ('stdout'); record%stdout_path = unquote(value)
            case ('stderr'); record%stderr_path = unquote(value)
            case ('result'); record%result_path = unquote(value)
            case ('exit-code'); read(value, *, iostat=ios) record%exit_code
            end select
        end do
        close (u)
    end subroutine

    subroutine write_run_record(self, record, ok)
        class(wb_manifest), intent(in) :: self
        type(wb_run_record), intent(in) :: record
        logical, intent(out), optional :: ok
        character(len=:), allocatable :: path, dir
        integer :: u, ios
        dir = trim(self%tracking_directory)//'/'//trim(record%id)
        call make_directory(dir)
        path = trim(dir)//'/status.toml'
        open (newunit=u, file=path, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write (u, '(A)') 'id = "'//toml_escape(record%id)//'"'
            write (u, '(A)') 'state = "'//toml_escape(record%state)//'"'
            write (u, '(A)') 'command = "'//toml_escape(record%command)//'"'
            write (u, '(A)') 'cwd = "'//toml_escape(record%cwd)//'"'
            write (u, '(A)') 'params = "'//toml_escape(record%params)//'"'
            write (u, '(A,I0)') 'exit-code = ', record%exit_code
            write (u, '(A)') 'stdout = "'//toml_escape(record%stdout_path)//'"'
            write (u, '(A)') 'stderr = "'//toml_escape(record%stderr_path)//'"'
            write (u, '(A)') 'result = "'//toml_escape(record%result_path)//'"'
            close (u)
        end if
        if (present(ok)) ok = ios == 0
    end subroutine

    subroutine add_template(self, name)
        class(wb_manifest), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer :: i
        if (.not. allocated(self%templates)) allocate(self%templates(WB_MAX_TEMPLATES))
        i = find_template(self, name)
        if (i > 0) return
        if (self%n_templates >= WB_MAX_TEMPLATES) return
        self%n_templates = self%n_templates + 1; i = self%n_templates
        self%templates(i)%name = trim(name)
        self%templates(i)%source_dir = 'src'
        self%templates(i)%main = ''
        self%templates(i)%dependencies = ''
        self%templates(i)%build = ''; self%templates(i)%run = ''; self%templates(i)%test = ''
    end subroutine

    subroutine add_sweep(self, name)
        class(wb_manifest), intent(inout) :: self
        character(len=*), intent(in) :: name
        if (.not. allocated(self%sweeps)) allocate(self%sweeps(WB_MAX_SWEEPS))
        if (find_sweep(self, name) > 0 .or. self%n_sweeps >= WB_MAX_SWEEPS) return
        self%n_sweeps = self%n_sweeps + 1
        self%sweeps(self%n_sweeps)%name = trim(name)
        self%sweeps(self%n_sweeps)%n_values = 0
    end subroutine

    subroutine set_manifest_key(self, section, key, raw)
        class(wb_manifest), intent(inout) :: self
        character(len=*), intent(in) :: section, key, raw
        character(len=:), allocatable :: value
        value = unquote(raw)
        select case (trim(section)//'.'//trim(key))
        case ('project.version'); self%version = value
        case ('project.name'); self%name = value
        case ('project.root'); self%root = value
        case ('project.default-template'); self%default_template = value
        case ('toolchain.compiler'); self%compiler = value
        case ('toolchain.flags'); self%compiler_flags = value
        case ('mpi.enabled'); self%mpi_enabled = parse_bool(raw)
        case ('mpi.launcher'); self%mpi_launcher = value
        case ('mpi.ranks'); self%mpi_ranks = parse_int(raw, self%mpi_ranks)
        case ('openmp.enabled'); self%openmp_enabled = parse_bool(raw)
        case ('openmp.threads'); self%openmp_threads = parse_int(raw, self%openmp_threads)
        case ('openmp.flags'); self%openmp_flags = value
        case ('scheduler.kind'); self%scheduler_kind = lower(value)
        case ('scheduler.command'); self%scheduler_command = value
        case ('scheduler.partition'); self%scheduler_partition = value
        case ('scheduler.account'); self%scheduler_account = value
        case ('scheduler.time'); self%scheduler_time = value
        case ('scheduler.cpus-per-task'); self%scheduler_cpus = parse_int(raw, self%scheduler_cpus)
        case ('commands.build'); self%build_command = value
        case ('commands.run'); self%run_command = value
        case ('commands.test'); self%test_command = value
        case ('tracking.directory'); self%tracking_directory = value
        case ('tracking.monitor-command'); self%monitor_command = value
        case ('results.format'); self%result_format = lower(value)
        case ('results.visualize-command'); self%visualize_command = value
        case ('results.browse-command'); self%browse_command = value
        case ('results.convergence-command'); self%convergence_command = value
        end select
    end subroutine

    subroutine set_template_key(t, key, raw)
        type(wb_template), intent(inout) :: t
        character(len=*), intent(in) :: key, raw
        character(len=:), allocatable :: value
        value = unquote(raw)
        select case (trim(key))
        case ('source-dir'); t%source_dir = value
        case ('main'); t%main = value
        case ('dependencies'); t%dependencies = value
        case ('build'); t%build = value
        case ('run'); t%run = value
        case ('test'); t%test = value
        end select
    end subroutine

    subroutine set_sweep_key(s, key, raw)
        type(wb_sweep), intent(inout) :: s
        character(len=*), intent(in) :: key, raw
        if (trim(key) == 'values') call parse_values(s, unquote(raw))
    end subroutine

    subroutine parse_values(s, text)
        type(wb_sweep), intent(inout) :: s
        character(len=*), intent(in) :: text
        integer :: i, start, n, p
        s%n_values = 0
        n = 1
        do i = 1, len_trim(text)
            if (text(i:i) == ',') n = n + 1
        end do
        if (n > WB_MAX_VALUES) n = WB_MAX_VALUES
        allocate(s%values(n))
        start = 1
        do i = 1, n
            p = index(text(start:), ',')
            if (p == 0) then
                s%values(i) = adjustl(trim(text(start:))); s%n_values = i; exit
            end if
            s%values(i) = adjustl(trim(text(start:start+p-2))); s%n_values = i
            start = start + p
        end do
    end subroutine

    function slurm_command(self, command) result(out)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: out
        out = trim(self%scheduler_command)
        if (len_trim(self%scheduler_partition) > 0) out = out//' --partition='//trim(self%scheduler_partition)
        if (len_trim(self%scheduler_account) > 0) out = out//' --account='//trim(self%scheduler_account)
        if (len_trim(self%scheduler_time) > 0) out = out//' --time='//trim(self%scheduler_time)
        out = out//' --cpus-per-task='//int_string(self%scheduler_cpus)//' --wrap="'//trim(command)//'"'
    end function

    integer function find_template(self, name) result(i)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: name
        i = 0
        do while (i < self%n_templates)
            i = i + 1
            if (lower(trim(self%templates(i)%name)) == lower(trim(name))) return
        end do
        i = 0
    end function
    integer function find_sweep(self, name) result(i)
        class(wb_manifest), intent(in) :: self
        character(len=*), intent(in) :: name
        i = 0
        do while (i < self%n_sweeps)
            i = i + 1
            if (lower(trim(self%sweeps(i)%name)) == lower(trim(name))) return
        end do
        i = 0
    end function
    pure logical function starts_with(text, prefix)
        character(len=*), intent(in) :: text, prefix
        starts_with = len(text) >= len(prefix) .and. text(:len(prefix)) == prefix
    end function
    pure function lower(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: i, c
        out = text
        do i = 1, len(text)
            c = iachar(out(i:i)); if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c+32)
        end do
    end function
    pure function strip_comment(text) result(out)
        character(len=*), intent(in) :: text
        character(len=2048) :: out
        integer :: p
        out = text; p = index(out, '#'); if (p > 0) out(p:) = ' '
    end function
    pure function unquote(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        character(len=2048) :: buf
        integer :: n
        buf = adjustl(trim(text)); n = len(buf)
        ! Formatted reads on Windows may retain a carriage return.
        do while (n > 0 .and. iachar(buf(n:n)) <= 32)
            n = n - 1
        end do
        if (n >= 2 .and. ((buf(1:1) == '"' .and. buf(n:n) == '"') .or. &
                          (buf(1:1) == "'" .and. buf(n:n) == "'"))) then
            out = buf(2:n-1)
        else if (n > 0) then
            out = buf(:n)
        else
            out = ''
        end if
    end function
    pure function toml_escape(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i
        out = ''
        do i = 1, len_trim(text)
            if (text(i:i) == '"') then
                out = out//'\"'
            else if (text(i:i) == '\') then
                out = out//'\\'
            else
                out = out//text(i:i)
            end if
        end do
    end function
    logical function parse_bool(text)
        character(len=*), intent(in) :: text
        parse_bool = lower(trim(unquote(text))) == 'true' .or. trim(unquote(text)) == '1'
    end function
    integer function parse_int(text, fallback)
        character(len=*), intent(in) :: text
        integer, intent(in) :: fallback
        integer :: ios
        parse_int = fallback; read (text, *, iostat=ios) parse_int
        if (ios /= 0) parse_int = fallback
    end function
    function int_string(value) result(out)
        integer, intent(in) :: value
        character(len=32) :: out
        write (out, '(I0)') value; out = adjustl(out)
    end function
    subroutine replace(text, needle, value)
        character(len=:), allocatable, intent(inout) :: text
        character(len=*), intent(in) :: needle, value
        integer :: p
        do
            p = index(text, needle); if (p == 0) exit
            text = text(:p-1)//value//text(p+len(needle):)
        end do
    end subroutine
    subroutine make_directory(path)
        character(len=*), intent(in) :: path
        call execute_command_line('mkdir -p "'//trim(path)//'" 2>nul || mkdir "'//trim(path)//'"', wait=.true.)
    end subroutine
    subroutine make_parent_directory(path)
        character(len=*), intent(in) :: path
        integer :: p
        p = max(index(path, '/', back=.true.), index(path, '\', back=.true.))
        if (p > 1) call make_directory(path(:p-1))
    end subroutine

end module aura_workbench
