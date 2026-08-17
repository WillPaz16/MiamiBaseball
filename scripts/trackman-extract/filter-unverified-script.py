import os

# Define the path to the consolidated folder
consolidated_dir = "New"  # Replace this with the path to your consolidated folder

# Loop through each month folder (01 to 07 in this case)
for month in range(1, 8):
    month_folder = os.path.join(consolidated_dir, f"{month:02}")
    
    if os.path.exists(month_folder):
        print(f"Processing month folder: {month_folder}")
        
        for file in os.listdir(month_folder):
            file_path = os.path.join(month_folder, file)
            
            # Check if "unverified" is in the file name
            if "unverified" in file.lower():
                print(f"Deleting file: {file_path}")  # Print the file being deleted if having issues
                os.remove(file_path)  # Delete the file
            else:
                print(f"Keeping file: {file_path}")  # Print the file being kept if having issues

print("Finished filtering out unverified files.")
