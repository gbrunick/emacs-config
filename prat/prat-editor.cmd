
@set LOCK_FILE=%~dp1.prat-editor-lock
@rem echo LOCK_FILE=%LOCK_FILE%

if exist "%LOCK_FILE%" (
  @echo SHOW_CURRENT_EDIT_BUFFER
  @echo GIT_OUTPUT_START:

  @echo Command failed because you are already editing a file for Git
  @echo in Emacs.  You must complete or cancel that edit before
  @echo you can start another interactive Git command.
  @echo.
  @exit /b 1
) else (
  @rem Create the lock file
  type nul >> "%LOCK_FILE%"
)

@rem File to edit
@echo File: %1
@echo Waiting for Emacs...

@rem Wait for Emacs send this signal
@waitfor EmacsEditDone

@echo GIT_OUTPUT_START:
