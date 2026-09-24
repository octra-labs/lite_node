# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import sys

if sys.version_info < (3, 10):
    sys.exit("status = refused reason = python_version minimum = 3.10")