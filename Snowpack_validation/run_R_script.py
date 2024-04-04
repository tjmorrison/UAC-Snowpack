import subprocess

# Define the path to your R script
r_script_path = './sp_validation.R'

# Command to run the R script using Rscript
command = ['Rscript', r_script_path]

try:
    # Run the R script using subprocess
    subprocess.run(command, check=True)
    print("R script execution completed successfully.")
except subprocess.CalledProcessError as e:
    print(f"Error executing R script: {e}")
