"""Portable Aura computational-physics workbench GUI.

The GUI delegates compilation, schedulers, plotting, and scientific file
formats to configured commands or optional Python packages. This keeps Aura's
Fortran binary small while still providing a usable desktop workbench.
"""
from __future__ import annotations

import json
import os
import subprocess
import threading
import tkinter as tk
from pathlib import Path
from tkinter import filedialog, messagebox, ttk

try:
    import tomllib
except ModuleNotFoundError:
    tomllib = None


class WorkbenchApp:
    def __init__(self, root: tk.Tk, manifest_path: Path):
        self.root = root
        self.root.title("Aura | Computational Physics Workbench")
        self.root.geometry("1120x720")
        self.manifest_path = manifest_path
        self.manifest = self.load_manifest()
        self.project = self.manifest.get("project", {})
        self.commands = self.manifest.get("commands", {})
        self.results = self.manifest.get("results", {})
        self.tracking = self.manifest.get("tracking", {})
        self.output = tk.Text(root, bg="#081525", fg="#d9f3ff", insertbackground="white")
        self.status_labels = {}
        self.build_ui()

    def load_manifest(self):
        if tomllib is None:
            raise RuntimeError("Python 3.11+ is required for TOML manifests")
        with self.manifest_path.open("rb") as handle:
            return tomllib.load(handle)

    def build_ui(self):
        style = ttk.Style()
        style.configure("Accent.TButton", padding=8)
        header = ttk.Frame(self.root, padding=12)
        header.pack(fill="x")
        ttk.Label(header, text="AURA", font=("Segoe UI", 18, "bold")).pack(side="left")
        ttk.Label(header, text="  COMPUTATIONAL PHYSICS WORKBENCH",
                  font=("Segoe UI", 11)).pack(side="left", pady=5)
        ttk.Button(header, text="Open manifest", command=self.open_manifest).pack(side="right")

        body = ttk.Panedwindow(self.root, orient="horizontal")
        body.pack(fill="both", expand=True, padx=12, pady=(0, 12))
        left = ttk.Frame(body, padding=8)
        right = ttk.Frame(body, padding=8)
        body.add(left, weight=1)
        body.add(right, weight=3)

        self.status_labels["project"] = ttk.Label(
            left, text=self.project.get("name", "Physics project"),
            font=("Segoe UI", 14, "bold"))
        self.status_labels["project"].pack(anchor="w", pady=(0, 8))
        self.status_labels["manifest"] = ttk.Label(left, text="")
        self.status_labels["manifest"].pack(anchor="w")
        self.status_labels["compiler"] = ttk.Label(left, text="")
        self.status_labels["compiler"].pack(anchor="w")
        mpi = self.manifest.get("mpi", {})
        omp = self.manifest.get("openmp", {})
        scheduler = self.manifest.get("scheduler", {})
        self.status_labels["mpi"] = ttk.Label(left, text="")
        self.status_labels["mpi"].pack(anchor="w")
        self.status_labels["openmp"] = ttk.Label(left, text="")
        self.status_labels["openmp"].pack(anchor="w")
        self.status_labels["scheduler"] = ttk.Label(left, text="")
        self.status_labels["scheduler"].pack(anchor="w", pady=(0, 12))

        actions = ttk.LabelFrame(left, text="Workflow", padding=8)
        actions.pack(fill="x")
        for label, stage in (("Build", "build"), ("Run", "run"), ("Test", "test"),
                             ("Visualize", "visualize"), ("Browse results", "browse"),
                             ("Check convergence", "monitor")):
            ttk.Button(actions, text=label, style="Accent.TButton",
                       command=lambda s=stage: self.execute(s)).pack(fill="x", pady=2)
        ttk.Button(actions, text="Preview (no execution)",
                   command=self.preview).pack(fill="x", pady=2)

        sweeps = ttk.LabelFrame(left, text="Parameter sweep", padding=8)
        sweeps.pack(fill="both", expand=True, pady=(12, 0))
        self.sweep_list = tk.Listbox(sweeps, height=8)
        self.sweep_list.pack(fill="both", expand=True)
        self.run_list = tk.Listbox(sweeps, height=6)
        self.run_list.pack(fill="both", expand=True, pady=(8, 0))

        self.output.pack(in_=right, fill="both", expand=True)
        self.refresh_manifest_state()
        self.write("Ready. Configure [results] commands for plots and HDF5/NetCDF browsing.\n")

    def write(self, text):
        self.output.insert("end", text)
        self.output.see("end")

    def open_manifest(self):
        selected = filedialog.askopenfilename(filetypes=[("TOML", "*.toml"), ("All files", "*.*")])
        if selected:
            self.manifest_path = Path(selected)
            try:
                self.manifest = self.load_manifest()
                self.refresh_manifest_state()
                self.write(f"\nLoaded {selected}\n")
            except Exception as exc:
                messagebox.showerror("Manifest error", str(exc))

    def refresh_manifest_state(self):
        self.project = self.manifest.get("project", {})
        self.commands = self.manifest.get("commands", {})
        self.results = self.manifest.get("results", {})
        self.tracking = self.manifest.get("tracking", {})
        if not self.status_labels:
            return
        mpi = self.manifest.get("mpi", {})
        omp = self.manifest.get("openmp", {})
        scheduler = self.manifest.get("scheduler", {})
        self.status_labels["project"].configure(
            text=self.project.get("name", "Physics project"))
        self.status_labels["manifest"].configure(text=f"Manifest: {self.manifest_path}")
        self.status_labels["compiler"].configure(
            text=f"Compiler: {self.manifest.get('toolchain', {}).get('compiler', '')}")
        self.status_labels["mpi"].configure(
            text=f"MPI: {'on' if mpi.get('enabled') else 'off'} ({mpi.get('ranks', 1)} ranks)")
        self.status_labels["openmp"].configure(
            text=f"OpenMP: {'on' if omp.get('enabled') else 'off'} ({omp.get('threads', 1)} threads)")
        self.status_labels["scheduler"].configure(
            text=f"Scheduler: {scheduler.get('kind', 'local')}")
        self.sweep_list.delete(0, "end")
        for name, data in self.manifest.get("sweep", {}).items():
            self.sweep_list.insert("end", f"{name}: {', '.join(map(str, data.get('values', [])))}")
        self.refresh_runs()

    def refresh_runs(self):
        self.run_list.delete(0, "end")
        directory = Path(self.tracking.get("directory", ".aura/runs"))
        if not directory.is_absolute():
            directory = Path(self.project.get("root", ".")) / directory
        if not directory.exists():
            return
        for status_path in sorted(directory.glob("*/status.toml")):
            record = self.read_run_record(status_path)
            self.run_list.insert(
                "end", f"{record.get('id', status_path.parent.name)}: "
                f"{record.get('state', 'unknown')} (exit {record.get('exit-code', '-1')})")

    @staticmethod
    def read_run_record(path):
        record = {}
        for line in path.read_text(encoding="utf-8").splitlines():
            if "=" not in line:
                continue
            key, value = line.split("=", 1)
            value = value.strip()
            if value.startswith('"') and value.endswith('"'):
                value = value[1:-1].replace('\\"', '"').replace("\\\\", "\\")
            record[key.strip()] = value
        return record

    def command_for(self, stage):
        if stage in self.commands:
            return self.commands[stage]
        mapping = {"visualize": "visualize-command", "browse": "browse-command",
                   "monitor": "convergence-command"}
        return self.results.get(mapping.get(stage, ""), "")

    def substitutions(self, command):
        toolchain = self.manifest.get("toolchain", {})
        mpi = self.manifest.get("mpi", {})
        omp = self.manifest.get("openmp", {})
        values = {
            "{name}": self.project.get("name", "project"),
            "{root}": self.project.get("root", "."),
            "{compiler}": toolchain.get("compiler", "gfortran"),
            "{flags}": toolchain.get("flags", ""),
            "{openmp_flags}": omp.get("flags", "") if omp.get("enabled") else "",
            "{omp_threads}": str(omp.get("threads", 1)),
            "{mpi_prefix}": f"{mpi.get('launcher', 'mpiexec')} -n {mpi.get('ranks', 1)} " if mpi.get("enabled") else "",
            "{omp_prefix}": f"OMP_NUM_THREADS={omp.get('threads', 1)} " if omp.get("enabled") else "",
            "{tracking_directory}": self.tracking.get("directory", ".aura/runs"),
            "{result_format}": self.results.get("format", "csv"),
        }
        template = self.manifest.get("templates", {}).get(
            self.project.get("default-template", ""), {})
        values["{source_dir}"] = template.get("source-dir", "src")
        values["{main}"] = template.get("main", "")
        values["{dependencies}"] = template.get("dependencies", "")
        for key, value in values.items():
            command = command.replace(key, str(value))
        return command.strip()

    def execute(self, stage):
        command = self.command_for(stage)
        if not command:
            if stage == "visualize":
                self.visualize_csv()
                return
            if stage == "browse":
                self.browse_scientific_file()
                return
            self.write(f"\nNo {stage} command configured.\n")
            return
        command = self.substitutions(command)
        self.write(f"\n$ {command}\n")
        threading.Thread(target=self.run_command, args=(command,), daemon=True).start()

    def preview(self):
        self.write("\nPreview (no commands executed):\n")
        for stage in ("build", "run", "test"):
            command = self.command_for(stage)
            self.write(f"{stage.capitalize()}: {self.substitutions(command) if command else '(not configured)'}\n")
        self.write(f"Sweep points: {self.sweep_count()}\n")

    def sweep_count(self):
        count = 1
        for data in self.manifest.get("sweep", {}).values():
            count *= max(1, len(data.get("values", [])))
        return count

    def visualize_csv(self):
        path = filedialog.askopenfilename(
            filetypes=[("CSV data", "*.csv"), ("All files", "*.*")])
        if not path:
            return
        try:
            import matplotlib.pyplot as plt
            import numpy as np
            data = np.genfromtxt(path, delimiter=",", names=True)
            names = data.dtype.names
            if not names or len(names) < 2:
                raise ValueError("CSV must have a header and at least two columns")
            plt.figure("Aura result: " + Path(path).name)
            for name in names[1:]:
                plt.plot(data[names[0]], data[name], label=name)
            plt.xlabel(names[0])
            plt.grid(True, alpha=0.25)
            plt.legend()
            plt.tight_layout()
            plt.show()
        except ImportError:
            messagebox.showerror("Visualization", "Install matplotlib and numpy, or configure results.visualize-command.")
        except Exception as exc:
            messagebox.showerror("Visualization", str(exc))

    def browse_scientific_file(self):
        path = filedialog.askopenfilename(
            filetypes=[("Scientific data", "*.h5 *.hdf5 *.nc *.nc4"), ("All files", "*.*")])
        if not path:
            return
        suffix = Path(path).suffix.lower()
        try:
            if suffix in (".h5", ".hdf5"):
                import h5py
                with h5py.File(path, "r") as handle:
                    self.write(f"\nHDF5: {path}\n")
                    handle.visititems(lambda name, obj: self.write(
                        f"  {name} {'[dataset]' if hasattr(obj, 'shape') else '[group]'}\n"))
            elif suffix in (".nc", ".nc4"):
                import netCDF4
                with netCDF4.Dataset(path) as handle:
                    self.write(f"\nNetCDF: {path}\nDimensions:\n")
                    for name, dim in handle.dimensions.items():
                        self.write(f"  {name}: {len(dim)}\n")
                    self.write("Variables:\n")
                    for name, variable in handle.variables.items():
                        self.write(f"  {name}: {variable.dimensions} {variable.dtype}\n")
            else:
                raise ValueError("Select an HDF5 or NetCDF file")
        except ImportError:
            package = "h5py" if suffix in (".h5", ".hdf5") else "netCDF4"
            messagebox.showerror("Scientific browser",
                                 f"Install {package}, or configure results.browse-command.")
        except Exception as exc:
            messagebox.showerror("Scientific browser", str(exc))

    def run_command(self, command):
        try:
            process = subprocess.Popen(command, shell=True, cwd=self.project.get("root", "."),
                                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                       text=True, bufsize=1)
            for line in process.stdout:
                self.root.after(0, self.write, line)
            code = process.wait()
            self.root.after(0, self.write, f"[exit {code}]\n")
        except OSError as exc:
            self.root.after(0, self.write, f"[failed: {exc}]\n")


def main():
    root = tk.Tk()
    path = Path(os.environ.get("AURA_WORKBENCH_MANIFEST", "aura-workbench.toml"))
    try:
        WorkbenchApp(root, path)
    except Exception as exc:
        root.withdraw()
        messagebox.showerror("Aura GUI", str(exc))
        root.destroy()
        return 1
    root.mainloop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
