
# SPA Application UI Tags

library(shiny)

logo <- tags$a(
  tags$img(
    src = "logo.png",
    style = "position: absolute; right: 10px; top: 5px; z-index: 1000; padding: 5px; margin-bottom: 20px;",
    width = "80"
  ),
  ""
)

custom_css <- tags$head(
  tags$style(HTML("
          /* General Button Styling */
          .btn {
            padding: 5px 10px; /* Adjust padding for thinner buttons */
            font-size: 12px;   /* Adjust font size for a sleeker look */
            width: 150px;      /* Set a custom width for longer buttons */
            }

               /* Primary Buttons */
          .btn-primary {
            background-color: rgba(12, 61, 18, 0.5) !important; /* Light transparent green */
            border-color: rgba(12, 61, 18, 0.8) !important; /* Green border */
            color: black !important; /* Default text color */
            transition: background-color 0.3s, color 0.3s, border-color 0.3s; /* Smooth transition */
          }

          .btn-primary:hover {
            background-color: #0C3D12 !important; /* Solid green on hover */
            color: white !important; /* White text */
            border-color: #0C3D12 !important; /* Dark green border */
          }

          /* Save Buttons */
          .btn-save {
            background-color: rgba(0, 64, 128, 0.5) !important; /* Light transparent blue */
            border-color: rgba(0, 64, 128, 0.8) !important; /* Blue border */
            color: black !important; /* Default text color */
            transition: background-color 0.3s, color 0.3s, border-color 0.3s; /* Smooth transition */
          }

          .btn-save:hover {
            background-color: #004080 !important; /* Solid blue on hover */
            color: white !important; /* White text */
            border-color: #004080 !important; /* Dark blue border */
          }

          /* Run Process Buttons */
          .btn-run {
            background-color: rgba(204, 153, 51, 0.6) !important; /* Light transparent muted gold */
            border-color: rgba(204, 153, 51, 0.9) !important; /* Muted gold border */
            color: black !important; /* Default text color */
            transition: background-color 0.3s, color 0.3s, border-color 0.3s; /* Smooth transition */
          }

          .btn-run:hover {
            background-color: #CC9933 !important; /* Solid muted gold on hover */
            color: white !important; /* White text */
            border-color: #CC9933 !important; /* Match border to background on hover */
          }

          /* Danger Buttons */
          .btn-danger {
            background-color: rgba(139, 0, 0, 0.5) !important; /* Light transparent dark red */
            border: 1px solid rgba(139, 0, 0, 0.8) !important; /* Light dark red border */
            color: black !important; /* Default text color */
            transition: background-color 0.3s, color 0.3s, border-color 0.3s; /* Smooth transition */
          }

          .btn-danger:hover {
            background-color: #8B0000 !important; /* Dark red on hover */
            color: white !important; /* White text */
            border-color: #8B0000 !important; /* Red border */
          }

        /* Modal Footer Danger Buttons */
         .modal-footer .btn-danger {
          background-color: rgba(139, 0, 0, 0.5) !important; /* Light transparent dark red */
          color: black !important; /* Default text color */
          border: 2px solid rgba(139, 0, 0, 0.8) !important; /* Border color */
        }

        .modal-footer .btn-danger:hover {
          background-color: #8B0000 !important; /* Dark red on hover */
          color: white !important; /* White text */
          border-color: #8B0000 !important; /* Red border */
        }

         #cancel_modal_button {
        background-color: rgba(139, 0, 0, 0.5); /* Light transparent dark red */
        color: white; /* Text color */
        border: 2px solid rgba(139, 0, 0, 0.8); /* Red border */
        font-size: 14px; /* Adjust font size */
        padding: 5px 10px; /* Adjust padding */
        cursor: pointer; /* Pointer cursor */
        transition: background-color 0.3s, color 0.3s, border-color 0.3s; /* Smooth transitions */
      }
      #cancel_modal_button:hover {
        background-color: #8B0000; /* Solid red on hover */
        border-color: #8B0000; /* Dark red border */
      }

      /* Light (Icon) Buttons */
      .btn-light {
        font-size: 14px; /* Small font size for the icon */
        color: rgba(108, 117, 125, 0.8); /* Neutral grey with transparency */
        background: none; /* Transparent background */
        border: none; /* No border */
        padding: 2px; /* Small padding to reduce button size */
        margin: 0; /* No margin */
        cursor: pointer; /* Pointer cursor for interactivity */
        transition: color 0.3s ease, background-color 0.3s ease; /* Smooth transitions */
      }
      .btn-light:hover {
        color: #6c757d; /* Solid neutral grey on hover */
        background-color: rgba(0, 0, 0, 0.1); /* Light grey background on hover */
      }
      .btn-light:focus {
        outline: none; /* Remove focus outline */
      }
      .btn-light .fa-gear {
        font-size: 16px; /* Ensure the gear icon stays consistent */
      }

      /* Hover Styling for General Buttons */
      .btn:hover {
        color: white; /* Ensure all buttons turn white text on hover */
      }

      /* Card Buttons */
      .card-buttons {
        display: flex; /* Flexbox for layout */
        justify-content: space-between; /* Space buttons evenly */
        align-items: center; /* Align items vertically */
        gap: 10px; /* Space between buttons */
      }
      .card-buttons .btn {
        flex: 1; /* Equal width for buttons */
        max-width: 33%; /* Limit button width to 33% of card */
      }

      /* File Input Container */
      .form-group {
        margin-bottom: 10px; /* Reduce spacing around inputs */
      }

      /* File Input Label Styling */
      .form-group label.control-label {
        font-size: 12px; /* Smaller label text size */
      }

      /* File Input Button Styling */
      .btn-file {
        height: 30px; /* Set a fixed height for the button */
        font-size: 12px; /* Smaller button text size */
        padding: 5px 10px; /* Adjust padding to reduce button height */
        line-height: 1.5; /* Adjust line height for text alignment */
        background-color: rgba(12, 61, 18, 0.5); /* Match primary color */
        color: black; /* Default text color */
        border: 1px solid rgba(12, 61, 18, 0.8); /* Match primary border */
        transition: background-color 0.3s, color 0.3s, border-color 0.3s;
      }
      .btn-file:hover {
        background-color: #0C3D12; /* Match hover color for primary */
        color: white;
        border-color: #0C3D12;
      }

        /* File Input Placeholder Text */
        .input-group .form-control {
          font-size: 10px; /* Smaller text size for placeholder */
          height: 30px; /* Match the height of the button */
          padding: 5px 5px; /* Adjust padding */
        }
        #species_stocking_summary_table {
            margin-top: 0px;
            margin-bottom: 0px;
          }
  "))
)
