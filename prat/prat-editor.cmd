@rem File to edit
@echo File: %1
@echo Waiting for Emacs...

@rem Wait for Emacs send this signal
@waitfor EmacsEditDone

@echo Command output:
